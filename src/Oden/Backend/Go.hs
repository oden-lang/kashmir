{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Oden.Backend.Go where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Set              as Set
import           Numeric
import           System.FilePath
import           Text.PrettyPrint
import           Text.Regex.PCRE.Heavy

import           Oden.Backend
import           Oden.Compiler.Monomorphization
import           Oden.Core
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo (SourceInfo(Missing))
import           Oden.Type.Basic
import qualified Oden.Type.Monomorphic as Mono

type Codegen = ReaderT MonomorphedPackage (Except CodegenError)

newtype GoBackend = GoBackend FilePath

block :: Doc -> Doc
block d = lbrace $+$ nest 4 d $+$ rbrace

funcArg :: Name -> Mono.Type -> Codegen Doc
funcArg name t = do
  tc <- codegenType t
  return $ safeName name <+> tc

func :: Doc -> Doc -> Doc -> Doc -> Doc
func name arg returnType body =
  text "func"
  <+> name
  <> parens arg
  <+> returnType
  <+> body

varWithType :: Name -> Mono.Type -> Expr Mono.Type -> Codegen Doc
varWithType name mt expr = do
  mtc <- codegenType mt
  ec <- codegenExpr expr
  return $ text "var"
           <+> safeName name
           <+> mtc
           <+> equals
           <+> ec

var :: Name -> Expr Mono.Type -> Codegen Doc
var name expr = varWithType name (typeOf expr) expr

return' :: Expr Mono.Type -> Codegen Doc
return' e@(Application _ f _ Mono.TUnit{}) = do
  ec <- codegenExpr e
  return $ case typeOf f of
    Mono.TUncurriedFn{} -> ec $+$ text "return struct{}{}"
    Mono.TVariadicFn{}  -> ec $+$ text "return struct{}{}"
    _                   -> text "return" <+> ec
return' e@(NoArgApplication _ f Mono.TUnit{}) = do
  ec <- codegenExpr e
  return $ case typeOf f of
    Mono.TUncurriedFn{} -> ec $+$ text "return struct{}{}"
    Mono.TVariadicFn{}  -> ec $+$ text "return struct{}{}"
    _                   -> text "return" <+> ec
return' e@(UncurriedFnApplication _ f _ Mono.TUnit{}) = do
  ec <- codegenExpr e
  return $ case typeOf f of
    Mono.TUncurriedFn{} -> ec $+$ text "return struct{}{}"
    Mono.TVariadicFn{}  -> ec $+$ text "return struct{}{}"
    _                   -> text "return" <+> ec
return' expr = do
  ec <- codegenExpr expr
  return $ text "return" <+> ec

replaceIdentifierPart :: Char -> String
replaceIdentifierPart '-' = "_DASH_"
replaceIdentifierPart '\'' = "_PRIM_"
replaceIdentifierPart '!' = "_BANG_"
replaceIdentifierPart '$' = "_DLLR_"
replaceIdentifierPart '&' = "_AMPR_"
replaceIdentifierPart '*' = "_STAR_"
replaceIdentifierPart '+' = "_PLUS_"
replaceIdentifierPart '<' = "_LT_"
replaceIdentifierPart '=' = "_EQ_"
replaceIdentifierPart '>' = "_GT_"
replaceIdentifierPart '?' = "_QST_"
replaceIdentifierPart '^' = "_CRCM_"
replaceIdentifierPart '|' = "_PIPE_"
replaceIdentifierPart '~' = "_TLDE_"
replaceIdentifierPart c = [c]

safeName :: Name -> Doc
safeName = text . concatMap replaceIdentifierPart

codegenIdentifier :: Identifier -> Codegen Doc
codegenIdentifier (Unqualified n) = return $ safeName n
codegenIdentifier (Qualified pn n) = return $ safeName pn <> text "." <> safeName n

codegenQualifiedName :: QualifiedName -> Codegen Doc
codegenQualifiedName (FQN pkgName name) = do
  (MonomorphedPackage (PackageDeclaration _ currentPkgName) _ _ _) <- ask
  if pkgName == currentPkgName
  then return $ safeName name
  else return $ safeName (last pkgName) <> text "." <> safeName name

codegenType :: Mono.Type -> Codegen Doc
codegenType Mono.TUnit{} = return $ text "struct{}"
codegenType (Mono.TBasic _ TInt) = return $ text "int"
codegenType (Mono.TBasic _ TBool) = return $ text "bool"
codegenType (Mono.TBasic _ TString) = return $ text "string"
codegenType (Mono.TTuple _ f s r) = do
  fs <- zipWithM codegenTupleField [0..] (f:s:r)
  return $ text "struct" <> braces (hcat (punctuate (text "; ") fs))
  where
  codegenTupleField :: Int -> Mono.Type -> Codegen Doc
  codegenTupleField n t = do
    tc <- codegenType t
    return $ text ("_" ++ show n) <+> tc
codegenType Mono.TAny{} = return $ text "interface{}"
codegenType (Mono.TCon _ _d _r) = throwError $ UnexpectedError "Type constructors not implemented yet."
codegenType (Mono.TNoArgFn _ f) = do
  fc <- codegenType f
  return $func empty empty fc empty
codegenType (Mono.TFn _ d r) = do
  dc <- codegenType d
  rc <- codegenType r
  return $func empty dc rc empty
codegenType (Mono.TSlice _ t) = do
  tc <- codegenType t
  return $ text "[]" <> tc
codegenType (Mono.TUncurriedFn _ as [r]) = do
  as' <- mapM codegenType as
  rc <- codegenType r
  return $ func empty (hcat (punctuate (comma <+> space) as')) rc empty
codegenType (Mono.TUncurriedFn _ as rs) = do
  as' <- mapM codegenType as
  rcs <- mapM codegenType rs
  return $ func empty (hcat (punctuate (comma <+> space) as')) (braces (hcat (punctuate (text ",") rcs))) empty
codegenType (Mono.TVariadicFn _ as v [r]) = do
  as' <- mapM codegenType as
  vc <- codegenType v
  rc <- codegenType r
  return $ func empty (hcat (punctuate (comma <+> space) (as' ++ [vc <> text "..."]))) rc empty
codegenType (Mono.TVariadicFn _ as v rs) = do
  as' <- mapM codegenType as
  vc <- codegenType v
  rcs <- mapM codegenType rs
  return $ func empty (hcat (punctuate (comma <+> space) (as' ++ [vc <> text "..."]))) (braces (hcat (punctuate (text ",") rcs))) empty
codegenType (Mono.TStruct _ fs) = (text "struct" <>) . block . vcat <$> (mapM codegenField fs)
  where codegenField (Mono.TStructField _ name t) = do
          tc <- codegenType t
          return $ safeName name <+> tc
codegenType (Mono.TNamed _ _ t) = codegenType t

showGoString :: Show a => a -> String
showGoString s = gsub ([re|(\\)(\d+)|]) toHex (show s)
  where
  toHex (_:n:_) = "\\U" ++ pad 8 (showHex (read n :: Int) "")
  toHex (m:_) = m
  toHex [] = ""
  pad :: Int -> String -> String
  pad n s'
      | length s' < n  = replicate (n - length s') '0' ++ s'
      | otherwise      = s'

codegenBinaryOperator :: BinaryOperator -> Doc
codegenBinaryOperator Add = text "+"
codegenBinaryOperator Subtract = text "-"
codegenBinaryOperator Multiply = text "*"
codegenBinaryOperator Divide = text "/"
codegenBinaryOperator Equals = text "=="
codegenBinaryOperator Concat = text "+"
codegenBinaryOperator LessThan = text "<"
codegenBinaryOperator GreaterThan = text ">"
codegenBinaryOperator LessThanEqual = text "<="
codegenBinaryOperator GreaterThanEqual = text ">="
codegenBinaryOperator And = text "&&"
codegenBinaryOperator Or = text "||"

codegenUnaryOperator :: UnaryOperator -> Doc
codegenUnaryOperator o = text (show o)

codegenRange :: Range Mono.Type -> Codegen Doc
codegenRange (Range e1 e2) =
  brackets . hcat . punctuate (text ":") <$> mapM codegenExpr [e1, e2]
codegenRange (RangeTo e) = do
  ec <- codegenExpr e
  return $ brackets $ text ":" <+> ec
codegenRange (RangeFrom e) = do
  ec <- codegenExpr e
  return $ brackets $ ec <+> (text ":")

-- | Generates a call to an uncurried function
codegenRawUncurredFnApplication :: Expr Mono.Type -> [Expr Mono.Type] -> Codegen Doc
codegenRawUncurredFnApplication f ps =
  case typeOf f of
    Mono.TVariadicFn{} ->
      let nonVariadicParams = init ps
          slice = last ps
      in do fc <-codegenExpr f
            pc <- mapM codegenExpr nonVariadicParams
            sc <- codegenExpr slice
            return $ fc <> parens (hcat (punctuate (comma <+> space) (pc ++ [sc <> text "..."])))
    _ -> do
      fc <- codegenExpr f
      pc <- mapM codegenExpr ps
      return $ fc <> parens (hcat (punctuate (comma <+> space) pc))

-- | Generates an anonymous function that will take arguments
-- | of the specified types and return a tuple containing those values
codegenToTupleWrapper :: Mono.Type -> Mono.Type -> [Mono.Type] -> Codegen Doc
codegenToTupleWrapper t1 t2 tr =
  let
    ts = t1:t2:tr
    argNames = take (length ts) $ map (\x -> "_" ++ show (x::Int)) [0..]
  in do
    fnArgs <- mapM (\(n, t) -> funcArg n t) (zip argNames ts)
    fnType <- codegenType (Mono.TTuple Missing t1 t2 tr)
    let fnBody = braces $ text "return" <+>
                          fnType <+>
                          braces (hcat (punctuate comma (map text argNames)))
    return $ func empty (hcat (punctuate comma fnArgs)) fnType fnBody

codegenExpr :: Expr Mono.Type -> Codegen Doc
codegenExpr (Symbol _ i _) =
  codegenIdentifier i
codegenExpr (Subscript _ s i _) = do
  sc <- codegenExpr s
  ic <- codegenExpr i
  return $ sc <> brackets ic
codegenExpr (Subslice _ s r _) =
  (<>) <$> codegenExpr s <*> codegenRange r
codegenExpr (UnaryOp _ o e _) =
  parens . (codegenUnaryOperator o <+>) <$> codegenExpr e
codegenExpr (BinaryOp _ o e1 e2 _) = do
  ec1 <- codegenExpr e1
  ec2 <- codegenExpr e2
  return $ parens (ec1 <+> codegenBinaryOperator o <+> ec2)
codegenExpr (Application _ (Symbol _ (Unqualified "not") _) e _) =
  (text "!" <>) <$> codegenExpr e
codegenExpr (Application _ f p _) =
  (<>) <$> codegenExpr f <*> (parens <$> codegenExpr p)

codegenExpr (UncurriedFnApplication _ f ps _) =
  case typeOf f of
    -- If there are more return values, we convert them to a tuple
    Mono.TUncurriedFn _ _ (t1:t2:tr) -> do
      fnCall <- codegenRawUncurredFnApplication f ps
      wrapperFn <- codegenToTupleWrapper t1 t2 tr
      return $ wrapperFn <+> (parens fnCall)
    Mono.TVariadicFn _ _ _ (t1:t2:tr) -> do
      fnCall <- codegenRawUncurredFnApplication f ps
      wrapperFn <- codegenToTupleWrapper t1 t2 tr
      return $ wrapperFn <+> (parens fnCall)
    -- Otherwise, just generate the call
    _ -> codegenRawUncurredFnApplication f ps

codegenExpr (NoArgApplication _ f _) =
  (<> parens empty) <$> codegenExpr f
codegenExpr (Fn _ (NameBinding _ a) body (Mono.TFn _ d r)) =
  func empty <$> funcArg a d <*> codegenType r <*> (braces <$> return' body)
codegenExpr (Fn _ _ _ t) = throwError $ UnexpectedError $ "Invalid fn type: " ++ show t
codegenExpr (NoArgFn _ body (Mono.TNoArgFn _ r)) =
  func empty empty <$> codegenType r <*> (braces <$> return' body)
codegenExpr (NoArgFn _ _ t) = throwError $ UnexpectedError $ "Invalid no-arg fn type: " ++ show t
codegenExpr (Let _ (NameBinding _ n) expr body t) = do
  tc <- codegenType t
  etc <- codegenType (typeOf expr)
  ec <- codegenExpr expr
  bc <- return' body
  return $ parens (func empty empty tc (braces (text "var" <+> safeName n <+> etc <+> equals <+> ec $+$ bc)))
           <> parens empty
codegenExpr (Literal _ (Int n) _) = return $ integer n
codegenExpr (Literal _ (Bool True) _) = return $ text "true"
codegenExpr (Literal _ (Bool False) _) = return $ text "false"
codegenExpr (Literal _ (String s) _) = return $ text (showGoString s)
codegenExpr (Literal _ Unit{} _) = return $ text "struct{}{}"
codegenExpr (Tuple _ f s r t) =
  (<>) <$> codegenType t <*> (braces . hcat . punctuate (comma <+> space) <$> (mapM codegenExpr (f:s:r)))
codegenExpr (If _ condExpr thenExpr elseExpr t) = do
  tc <- codegenType t
  condc <- codegenExpr condExpr
  thenc <- block <$> return' thenExpr
  elsec <- block <$> return' elseExpr
  return $ parens (func empty empty tc (braces (text "if" <+> condc <+> thenc <+> text "else" <+> elsec)))
           <> parens empty
codegenExpr (Slice _ exprs t) =
  (<>) <$> codegenType t <*> (braces . hcat . punctuate (comma <+> space) <$> (mapM codegenExpr exprs))
codegenExpr (Block _ [] t) = do
  tc <- codegenType t
  return $ parens
           (func empty empty tc (braces empty))
           <> parens empty
codegenExpr (Block _ exprs t) = do
  tc <- codegenType t
  ic <- mapM codegenExpr (init exprs)
  rc <- return' (last exprs)
  return $ parens
           (func empty empty tc (braces (vcat (ic ++ [rc]))))
           <> parens empty
codegenExpr (StructInitializer _ structType values) = do
  tc <- codegenType structType
  vc <- mapM codegenExpr values
  return $ tc <> braces (hcat (punctuate (comma <+> space) vc))

codegenTopLevel :: Name -> Mono.Type -> Expr Mono.Type -> Codegen Doc
codegenTopLevel "main" (Mono.TNoArgFn _ Mono.TUnit{}) (NoArgFn _ body _) =
  func (text "main") empty empty <$> (braces <$> codegenExpr body)
codegenTopLevel name (Mono.TNoArgFn _ r) (NoArgFn _ body _) =
  func (safeName name) empty <$> codegenType r <*> (braces <$> return' body)
codegenTopLevel name _ (NoArgFn _ body (Mono.TNoArgFn _ r)) =
  func (safeName name) empty <$> codegenType r <*> (braces <$> return' body)
codegenTopLevel name (Mono.TFn _ d r) (Fn _ (NameBinding _ a) body _) =
  func (safeName name) <$> funcArg a d <*> codegenType r <*> (braces <$> return' body)
codegenTopLevel name _ (Fn _ (NameBinding _ a) body (Mono.TFn _ d r)) =
  func (safeName name) <$> funcArg a d <*> codegenType r <*> (braces <$> return' body)
codegenTopLevel name t expr =
  varWithType name t expr

codegenInstance :: InstantiatedDefinition -> Codegen Doc
codegenInstance (InstantiatedDefinition name expr) =
  codegenTopLevel name (typeOf expr) expr

codegenMonomorphed :: MonomorphedDefinition -> Codegen Doc
codegenMonomorphed (MonomorphedDefinition _ name mt expr) =
  codegenTopLevel name mt expr

codegenImport :: Import -> Doc
codegenImport (Import _ name) =
  text "import" <+> doubleQuotes (hcat (punctuate (text "/") (map text name)))

codegenPackage :: MonomorphedPackage -> Codegen Doc
codegenPackage (MonomorphedPackage (PackageDeclaration _ name) imports is ms) = do
  isc <- mapM codegenInstance (Set.toList is)
  msc <- mapM codegenMonomorphed (Set.toList ms)
  return $ text "package" <+> text (last name)
           $+$ vcat (map codegenImport imports)
           $+$ vcat isc
           $+$ vcat msc

toFilePath :: GoBackend -> PackageName -> Codegen FilePath
toFilePath _ [] = throwError (UnexpectedError "Package name cannot be empty")
toFilePath (GoBackend goPath) parts =
  let isMain = last parts == "main"
      dirParts = "src" : if isMain then init parts else parts
      dir = foldl (</>) goPath dirParts
      fileName = if isMain then "main.go" else last parts ++ ".go"
  in return (dir </> fileName)

instance Backend GoBackend where
  codegen backend pkg@(MonomorphedPackage (PackageDeclaration _ name) _ _ _) =
    runExcept (runReaderT action pkg)
    where action = do
            path <- toFilePath backend name
            code <- codegenPackage pkg
            return [CompiledFile path (render code)]
