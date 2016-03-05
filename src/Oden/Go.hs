-- | This module queries the Go importer for available definitions in Go
-- packages in the system. All Go types and constructs are not support by Oden
-- (yet) so it also returns 'UnsupportedTypesWarning's for those types which
-- it ignores.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Oden.Go (
  PackageImportError(..),
  UnsupportedTypesWarning(..),
  getPackageDefinitions
) where

import qualified Oden.Core                  as Core
import           Oden.Go.Types              as G
import           Oden.Identifier
import           Oden.QualifiedName         (QualifiedName(..))
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic      as Poly
import           Oden.Type.Basic

import           Control.Applicative        hiding (Const)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map                   as Map
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

instance FromJSON Type where
  parseJSON (Object o) = do
    kind :: String <- o .: "kind"
    case kind of
      "basic"         -> Basic <$> o .: "name" <*> o .: "untyped"
      "pointer"       -> Pointer <$> o .: "inner"
      "interface"     -> return Interface
      "array"         -> G.Array <$> o .: "length"
                                 <*> o .: "inner"
      "slice"         -> Slice <$> o .: "inner"
      "signature"     -> Signature <$> o .: "variadic"
                                   <*> o `optOrNull` "recv"
                                   <*> o .: "arguments"
                                   <*> o .: "returns"
      "struct"        -> Struct <$> o .: "fields"
      "named"         -> Named <$> o .: "pkg" <*> o .: "name" <*> o .: "underlying"
      "unsupported"   -> Unsupported <$> o .: "name"
      k -> fail ("Unknown kind: " ++ k)
  parseJSON v = fail ("Unexpected: " ++ show v)

data PackageObject = Func Name Type
                   | Var Name Type
                   | Const Name Type
                   | NamedType Name Type
                   deriving (Show, Eq)

nameOf :: PackageObject -> Name
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

data PackageImportError = PackageImportError Core.PackageName String deriving (Show, Eq)

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

data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: Core.PackageName
                                                       , messages :: [(Name, String)]
                                                       } deriving (Show, Eq)

convertType :: G.Type -> Either String Poly.Type
-- TODO: Add "Untyped constant" concept in Oden type system
-- and/or consider how macros would relate to this.
convertType (Basic "bool" False) = return (Poly.TBasic Missing TBool)
convertType (Basic "int" False) = return (Poly.TBasic Missing TInt)
convertType (Basic "rune" False) = return (Poly.TBasic Missing TInt)
convertType (Basic "string" False) = return (Poly.TBasic Missing TString)
convertType (Basic "nil" False) = Left "nil constants"
convertType (Basic n False) = Left ("Basic type: " ++ n)
convertType (Basic n True) = Left ("Basic untyped: " ++ n)
convertType (Pointer _) = Left "Pointers"
convertType (G.Array _ _) = Left "Arrays"
convertType (Slice t) = Poly.TSlice Missing <$> convertType t
convertType Interface{} = Right $ Poly.TAny Missing
convertType (Signature _ (Just _) _ _) = Left "Methods (functions with receivers)"
convertType (Signature False Nothing args []) = do
  as <- mapM convertType args
  Right (Poly.TUncurriedFn Missing as [Poly.TUnit Missing])
convertType (Signature False Nothing args ret) = do
  as <- mapM convertType args
  r <- mapM convertType ret
  Right (Poly.TUncurriedFn Missing as r)
convertType (Signature True Nothing [] []) = Left "Variadic functions with no arguments"
convertType (Signature True Nothing args []) = do
  as <- mapM convertType (init args)
  v <- convertType (last args)
  Right (Poly.TVariadicFn Missing as v (Poly.TUnit Missing))
convertType (Signature True Nothing args [ret]) = do
  as <- mapM convertType (init args)
  v <- convertType (last args)
  r <- convertType ret
  Right (Poly.TVariadicFn Missing as v r)
convertType (Signature True Nothing args _) = do
  as <- mapM convertType (init args)
  v <- convertType (last args)
  Right (Poly.TVariadicFn Missing as v (Poly.TUnit Missing))
-- convertType (Signature _ Nothing _ _) = Left "Functions with multiple return values"
convertType (Named pkgName n (Struct fields)) = do
  fields' <- mapM convertField fields
  return (Poly.TNamed Missing (FQN pkgName n) (Poly.TStruct Missing fields'))
  where
  convertField (StructField name goType) = Poly.TStructField Missing name <$> convertType goType
convertType (Named _ _ t) = convertType t
convertType (Struct fields) = do
  fields' <- mapM convertStructField fields
  return (Poly.TStruct Missing fields')
  where
  convertStructField (StructField fieldName goType) = Poly.TStructField Missing fieldName <$> convertType goType
convertType (Unsupported n) = Left n

objectsToScope :: Core.PackageName -> [PackageObject] -> (Map.Map Name Core.Definition, Maybe UnsupportedTypesWarning)
objectsToScope pkgName objs =
  case foldl addObject ([], []) objs of
    (s, []) -> (Map.fromList s, Nothing)
    (s, msgs) -> (Map.fromList s, Just (UnsupportedTypesWarning pkgName msgs))
  where
  addObject (assocs, us) (NamedType name goType) =
    case convertType goType of
         Left u -> (assocs, (name, u) : us)
         Right type' ->
           ((name, Core.TypeDefinition Missing (FQN pkgName name) [] type') : assocs, us)
  addObject (assocs, us) o =
    let n = nameOf o
        t = typeOf o
    in case convertType t of
         Left u -> (assocs, (n, u) : us)
         Right ct ->
           let sc = Poly.Forall Missing [] ct
           in ((n, Core.ForeignDefinition Missing n sc) : assocs, us)

getPackageDefinitions :: Core.PackageName -> IO (Either PackageImportError (Map.Map Name Core.Definition, Maybe UnsupportedTypesWarning))
getPackageDefinitions pkgName = do
  cs <- newCString (intercalate "/" pkgName)
  objs <- decodeResponse pkgName <$> (c_GetPackageObjects cs >>= peekCString)
  return (objectsToScope pkgName <$> objs)
