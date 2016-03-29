-- | This module queries the Go importer for available definitions in Go
-- packages in the system and converts those definitions to the Oden type
-- system.
--
-- All Go types and constructs are not support by Oden (yet) so it returns
-- 'UnsupportedTypesWarning's for those types which it ignores.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Oden.Go.Importer (
  PackageImportError(..),
  UnsupportedTypesWarning(..),
  importer
) where

import qualified Oden.Core                  as Core
import           Oden.Go.Type               as G
import qualified Oden.Go.Identifier         as GI
import           Oden.Identifier
import           Oden.Imports
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName         (QualifiedName(..))
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic      as Poly

import           Control.Applicative        hiding (Const)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (intercalate)
import qualified Data.Text                  as T

import           Foreign.C.String
foreign import ccall "GetPackageObjects" c_GetPackageObjects :: CString -> IO CString

optOrNull :: FromJSON a => HM.HashMap T.Text Value -> T.Text -> Parser (Maybe a)
o `optOrNull` k = case HM.lookup k o of
                    Just Null -> return Nothing
                    Just v -> parseJSON v
                    Nothing -> return Nothing

instance FromJSON StructField where
  parseJSON (Object o) = StructField <$> o .: "name" <*> o .: "type"
  parseJSON v = fail ("Unexpected: " ++ show v)

instance FromJSON GI.Identifier where
  parseJSON (String s) = return (GI.Identifier (T.unpack s))
  parseJSON v = fail ("Expecting a string but got: " ++ show v)

instance FromJSON Type where
  parseJSON (Object o) = do
    kind :: String <- o .: "kind"
    case kind of
      "basic"         -> Basic <$> o .: "name" <*> o .: "untyped"
      "pointer"       -> Pointer <$> o .: "inner"
      "interface"     -> return (Interface [])
      "array"         -> G.Array <$> o .: "length"
                                 <*> o .: "inner"
      "slice"         -> Slice <$> o .: "inner"
      "signature"     -> do
        isVariadic <- o .: "variadic"
        receiver <- o `optOrNull` "recv"
        params <- o .: "params"
        returns <- o .: "returns"
        return (Signature receiver (Parameters params isVariadic) (Returns returns))
      "struct"        -> Struct <$> o .: "fields"
      "named"         -> Named <$> o .: "pkg" <*> o .: "name" <*> o .: "underlying"
      "unsupported"   -> Unsupported <$> o .: "name"
      k -> fail ("Unknown kind: " ++ k)
  parseJSON v = fail ("Unexpected: " ++ show v)

data PackageObject = Func String Type
                   | Var String Type
                   | Const String Type
                   | NamedType String Type
                   deriving (Show, Eq)

nameOf :: PackageObject -> String
nameOf (Func n _) = n
nameOf (Var n _) = n
nameOf (Const n _) = n
nameOf (NamedType n _ ) = n

typeOf :: PackageObject -> Type
typeOf (Func _ t) = t
typeOf (Var _ t) = t
typeOf (Const _ t) = t
typeOf (NamedType _ t) = t

instance FromJSON PackageObject where
  parseJSON (Object o) = do
    t :: String <- o .: "objectType"
    case t of
      "func"       -> Func <$> o .: "name" <*> o .: "type"
      "var"        -> Var <$> o .: "name" <*> o .: "type"
      "const"      -> Const <$> o .: "name" <*> o .: "type"
      "named_type" -> NamedType <$> o .: "name" <*> o .: "type"
      _ -> fail ("Unknown object type: " ++ t)
  parseJSON v = fail $ "Expected JSON object for PackageObject but got: " ++ show v

data PackageObjectsResponse = ErrorResponse String | ObjectsResponse [PackageObject]
                            deriving (Show, Eq)

instance FromJSON PackageObjectsResponse where
  parseJSON (Object o) = ErrorResponse <$> o .: "error"
                       <|> ObjectsResponse <$> o .: "objects"
  parseJSON v = fail $ "Expected JSON object for PackageObjectsResponse but got: " ++ show v

decodeResponse :: Core.PackageName -> String -> Either PackageImportError [PackageObject]
decodeResponse pkgName s = either (Left . PackageImportError pkgName) Right $ do
  value <- eitherDecode (pack s)
  case value of
    ErrorResponse err -> Left err
    ObjectsResponse objs -> Right objs

missing :: Metadata SourceInfo
missing = Metadata Missing

type Converter = StateT Int (Except String)

fresh :: Converter Poly.TVar
fresh = do
  n <- get
  modify (+ 1)
  return (Poly.TV ("_g" ++ show n))

convertType :: G.Type -> Converter Poly.Type
-- TODO: Add, or map, "Untyped constant" concept to Oden type system.
convertType (Basic (GI.Identifier "bool") False) = return typeBool
convertType (Basic (GI.Identifier "int") False) = return typeInt
convertType (Basic (GI.Identifier "rune") False) = return typeInt
convertType (Basic (GI.Identifier "string") False) = return typeString
convertType (Basic (GI.Identifier "nil") False) = throwError "nil constants"
convertType (Basic n False) = throwError ("Basic type: " ++ show n)
convertType (Basic n True) = throwError ("Basic untyped: " ++ show n)
convertType (Pointer _) = throwError "Pointers"
convertType (G.Array _ _) = throwError "Arrays"
convertType (Slice t) = Poly.TSlice missing <$> convertType t
convertType Interface{} = do
  t <- fresh
  return $ Poly.TVar missing t
convertType (Signature (Just _) _ _) = throwError "Methods (functions with receivers)"
convertType (Signature Nothing (Parameters params isVariadic) (Returns ret)) = do
  ps <- mapM convertType params
  catchError (wrapReturns ps <$> mapM convertType ret)
             (const $ return (Poly.TForeignFn missing isVariadic ps [typeUnit]))
  where
  wrapReturns ps [] = Poly.TForeignFn missing isVariadic ps [typeUnit] -- no return type
  wrapReturns ps rs = Poly.TForeignFn missing isVariadic ps rs
convertType (Named pkgName (GI.Identifier n) t@Struct{}) =
  Poly.TNamed missing (FQN pkgName (Identifier n)) <$> convertType t
convertType (Named _ _ t) = convertType t
convertType (Struct fields) = do
  fields' <- foldM convertField (Poly.REmpty (Metadata Missing)) fields
  return (Poly.TRecord missing fields')
  where
  convertField row (StructField (GI.Identifier name) goType) =
    Poly.RExtension missing (Identifier name) <$> convertType goType <*> return row
convertType (Unsupported n) = throwError n

objectsToPackage :: Core.PackageName
             -> [PackageObject]
             -> (Core.Package, [UnsupportedMessage])
objectsToPackage pkgName objs =
  (Core.Package (Core.PackageDeclaration missing pkgName) [] allDefs, allMessages)
  where
  (allDefs, allMessages) = foldl addObject ([], []) objs
  addObject (defs, msgs) (NamedType name goType) =
    let identifier = Identifier name in
    case runExcept (runStateT (convertType goType) 0) of
         Left u -> (defs, (identifier, u) : msgs)
         Right (type', _) ->
           (Core.TypeDefinition missing (FQN pkgName identifier) [] type' : defs, msgs)
  addObject (defs, msgs) obj =
    let n = Identifier (nameOf obj)
    in case runExcept (runStateT (convertType $ typeOf obj) 0) of
         Left u -> (defs, (n, u) : msgs)
         Right (ct, _) ->
           let sc = Poly.Forall missing [] ct
           in (Core.ForeignDefinition missing n sc : defs, msgs)

importer :: Importer
importer pkgName = do
  cs <- newCString (intercalate "/" pkgName)
  objs <- decodeResponse pkgName <$> (c_GetPackageObjects cs >>= peekCString)
  return (objectsToPackage pkgName <$> objs)
