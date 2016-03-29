{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Oden.Backend.Go where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.List             (sortOn, find, intercalate)
import           Data.Maybe            (maybeToList)
import qualified Data.Set              as Set

import           Text.PrettyPrint.Leijen (renderPretty, displayS, pretty)
import           System.FilePath

import qualified Oden.Go.AST           as AST
import qualified Oden.Go.Identifier    as GI
import qualified Oden.Go.Type          as GT
import           Oden.Go.Pretty ()

import           Oden.Backend
import           Oden.Compiler.Monomorphization
import           Oden.Core
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName (QualifiedName(..))
import           Oden.SourceInfo       hiding (fileName)
import qualified Oden.Type.Monomorphic as Mono

type Codegen = ReaderT MonomorphedPackage (Except CodegenError)

newtype GoBackend = GoBackend FilePath

isUniverseTypeConstructor :: String -> Mono.Type -> Bool
isUniverseTypeConstructor expected (Mono.TCon _ (FQN [] (Identifier actual))) =
  actual == expected
isUniverseTypeConstructor _ _ = False

replaceIdentifierPart :: Char -> String
replaceIdentifierPart c = case c of
  '-'  -> "_DASH_"
  '\'' -> "_PRIM_"
  '!'  -> "_BANG_"
  '$'  -> "_DLLR_"
  '&'  -> "_AMPR_"
  '*'  -> "_STAR_"
  '+'  -> "_PLUS_"
  '<'  -> "_LT_"
  '='  -> "_EQ_"
  '>'  -> "_GT_"
  '?'  -> "_QST_"
  '^'  -> "_CRCM_"
  '|'  -> "_PIPE_"
  '~'  -> "_TLDE_"
  _    -> [c]

safeName :: String -> GI.Identifier
safeName = GI.Identifier . concatMap replaceIdentifierPart

genIdentifier :: Identifier -> Codegen GI.Identifier
genIdentifier (Identifier s) = return $ safeName s

genType :: Mono.Type -> Codegen GT.Type
genType t@Mono.TCon{}
  | isUniverseTypeConstructor "unit" t = return $ GT.Struct []
  | isUniverseTypeConstructor "int" t = return $ GT.Basic (GI.Identifier "int") False
  | isUniverseTypeConstructor "bool" t = return $ GT.Basic (GI.Identifier "bool") False
  | isUniverseTypeConstructor "string" t = return $ GT.Basic (GI.Identifier "string") False
  | otherwise = throwError (UnexpectedError $ "Unsupported type constructor: " ++ show t)
genType (Mono.TTuple _ f s r) =
  GT.Struct <$> zipWithM genTupleField [0..] (f:s:r)
  where
  genTupleField :: Int -> Mono.Type -> Codegen GT.StructField
  genTupleField n t = GT.StructField (GI.Identifier ("_" ++ show n)) <$> genType t
genType (Mono.TNoArgFn _ f) = do
  fc <- genType f
  return (GT.Signature Nothing (GT.Parameters [] False) (GT.Returns [fc]))
genType (Mono.TFn _ d r) = do
  dc <- genType d
  rc <- genType r
  return (GT.Signature Nothing (GT.Parameters [dc] False) (GT.Returns [rc]))
genType (Mono.TSlice _ t) =
  GT.Slice <$> genType t
genType (Mono.TForeignFn _ isVariadic ps rs) = do
  ps' <- mapM genType ps
  rs' <- mapM genType rs
  return (GT.Signature Nothing (GT.Parameters ps' isVariadic) (GT.Returns rs'))
genType (Mono.TRecord _ row) = genType row
genType (Mono.TNamed _ _ t) = genType t
genType Mono.REmpty{} = return $ GT.Struct []
genType row@Mono.RExtension{} = do
  let fields = sortOn fst (Mono.rowToList row)
  GT.Struct <$> mapM genField fields
  where genField (Identifier name, t) = GT.StructField (safeName name) <$> genType t

genBinaryOperator :: BinaryOperator -> AST.BinaryOperator
genBinaryOperator op = case op of
  Add -> AST.Sum
  Subtract -> AST.Difference
  Multiply -> AST.Product
  Divide -> AST.Quotient
  Equals -> AST.EQ
  Concat -> AST.Sum
  LessThan -> AST.LT
  GreaterThan -> AST.GT
  LessThanEqual -> AST.LTE
  GreaterThanEqual -> AST.GTE
  And -> AST.And
  Or -> AST.Or

genUnaryOperator :: UnaryOperator -> AST.UnaryOperator
genUnaryOperator Negative = AST.UnaryNegation
genUnaryOperator Positive = AST.UnaryPlus
genUnaryOperator Not = AST.Not

genRange :: Range Mono.Type -> Codegen AST.SliceExpression
genRange (Range e1 e2) = AST.ClosedSlice <$> genExpr e1 <*> genExpr e2
genRange (RangeFrom e) = AST.LowerBoundSlice <$> genExpr e
genRange (RangeTo e) = AST.UpperBoundSlice <$> genExpr e

-- | Generates a call to an foreign function.
genRawForeignFnApplication :: Expr Mono.Type -> [Expr Mono.Type] -> Codegen AST.PrimaryExpression
genRawForeignFnApplication f args =
  case typeOf f of
    Mono.TForeignFn _ True _ _ -> do
      let nonVariadicArgs = init args
          slice = last args
      fc <- genPrimaryExpression f
      args' <- map AST.Argument <$> mapM genExpr nonVariadicArgs
      variadicArg <- AST.VariadicSliceArgument <$> genExpr slice
      return (AST.Application fc (args' ++ [variadicArg]))
    _ -> AST.Application <$> genPrimaryExpression f
                         <*> (map AST.Argument <$> mapM genExpr args)

operandName :: GI.Identifier -> AST.Expression
operandName = AST.Expression . AST.Operand . AST.OperandName

literalExpr :: AST.Literal -> AST.Expression
literalExpr = AST.Expression . AST.Operand . AST.Literal

-- | Generates an anonymous function that will take parameters of the specified
-- types and return a tuple containing those values.
genTupleWrapper :: (Mono.Type, Mono.Type, [Mono.Type]) -> Codegen AST.PrimaryExpression
genTupleWrapper (fstType, sndType, restTypes) = do
  let types = fstType : sndType : restTypes
      names = take (length types) (map (GI.Identifier . ("_" ++) . show) [(0 :: Int)..])

  paramTypes <- mapM genType types

  -- TODO: Create this type out of 'names' and AST constructors directly, don't
  -- genType to get it.
  tupleType <- genType (Mono.TTuple (Metadata Missing) fstType sndType restTypes)

  let params = zipWith AST.FunctionParameter names paramTypes
      tupleLiteral = AST.Expression
                     (AST.Operand
                      (AST.Literal
                       (AST.CompositeLiteral
                        tupleType
                        (AST.LiteralValueElements
                         (map (AST.UnkeyedElement . operandName) names)))))
  return (AST.Operand
          (AST.Literal
           (AST.FunctionLiteral
            (AST.FunctionSignature params [tupleType])
            (AST.Block [AST.ReturnStmt [tupleLiteral]]))))

-- | Generates a call to a foreign function with multiple return values,
-- returning a tuple.
genTupleWrappedForeignFnApplication :: Expr Mono.Type                      -- the function expr
                                    -> [Expr Mono.Type]                    -- function application arguments
                                    -> (Mono.Type, Mono.Type, [Mono.Type]) -- tuple types
                                    -> Codegen AST.Expression
genTupleWrappedForeignFnApplication f args returnTypes = do
  fnCall <- genRawForeignFnApplication f args
  wrapperFn <- genTupleWrapper returnTypes
  return (AST.Expression
          (AST.Application
           wrapperFn
           [AST.Argument (AST.Expression fnCall)]))

emptyStructLiteral :: AST.Expression
emptyStructLiteral =
  literalExpr
  (AST.CompositeLiteral
    (GT.Struct [])
    (AST.LiteralValueElements []))

-- | Wraps the provided code in a function that returns unit
voidToUnitWrapper :: AST.Expression -> AST.Expression
voidToUnitWrapper expr =
  AST.Expression
  (AST.Application
   (AST.Operand
    (AST.Literal
     (AST.FunctionLiteral
      (AST.FunctionSignature [] [GT.Struct []])
      (AST.Block [AST.SimpleStmt (AST.ExpressionStmt expr),
                  AST.ReturnStmt [emptyStructLiteral]]))))
   [])

asPrimaryExpression :: AST.Expression -> AST.PrimaryExpression
asPrimaryExpression (AST.Expression primaryExpr) = primaryExpr
asPrimaryExpression nonPrimary = AST.Operand (AST.GroupedExpression nonPrimary)

genPrimaryExpression :: Expr Mono.Type -> Codegen AST.PrimaryExpression
genPrimaryExpression expr = asPrimaryExpression <$> genExpr expr

returnSingle :: AST.Expression -> AST.Stmt
returnSingle expr = AST.ReturnStmt [expr]

genExpr :: Expr Mono.Type -> Codegen AST.Expression
genExpr expr = case expr of
  Symbol _ i _ -> (AST.Expression . AST.Operand . AST.OperandName) <$> genIdentifier i
  Subscript _ s i _ ->
    AST.Expression <$> (AST.Index <$> genPrimaryExpression s <*> genExpr i)
  Subslice _ s r _ ->
    AST.Expression <$> (AST.Slice <$> genPrimaryExpression s <*> genRange r)
  UnaryOp _ o e _ ->
    AST.UnaryOp (genUnaryOperator o) <$> genPrimaryExpression e
  BinaryOp _ o lhs rhs _ ->
    AST.BinaryOp (genBinaryOperator o) <$> genExpr lhs <*> genExpr rhs
  Application _ f arg _ ->
    AST.Expression <$> (AST.Application <$> genPrimaryExpression f
                                        <*> ((:[]) <$> AST.Argument <$> genExpr arg))
  ForeignFnApplication _ f args _ ->
    case typeOf f of

      -- Go functions that return unit are (almost always) void functions
      -- we wrap them to make the call to them return unit
      Mono.TForeignFn _ _ _ [t] | isUniverseTypeConstructor "unit" t -> do
        fnCall <- genRawForeignFnApplication f args
        return $ voidToUnitWrapper (AST.Expression fnCall)

      Mono.TForeignFn _ _ _ (t1:t2:tr) ->
        genTupleWrappedForeignFnApplication f args (t1, t2, tr)

      -- Otherwise, just generate the call
      _ -> AST.Expression <$> genRawForeignFnApplication f args

  NoArgApplication _ f _->
    AST.Expression <$> (AST.Application <$> genPrimaryExpression f <*> return [])

  Fn _ (NameBinding _ paramName) body (Mono.TFn _ d r) -> do
    param <- AST.FunctionParameter <$> genIdentifier paramName <*> genType d
    returnType <- genType r
    returnStmt <- returnSingle <$> genExpr body
    return (literalExpr (AST.FunctionLiteral (AST.FunctionSignature [param] [returnType]) (AST.Block [returnStmt])))
  Fn _ _ _ t ->
    throwError $ UnexpectedError $ "Invalid fn type: " ++ show t

  NoArgFn _ body (Mono.TNoArgFn _ r) -> do
    returnType <- genType r
    returnStmt <- returnSingle <$> genExpr body
    return (literalExpr (AST.FunctionLiteral (AST.FunctionSignature [] [returnType]) (AST.Block [returnStmt])))

  NoArgFn _ _ t ->
    throwError $ UnexpectedError $ "Invalid no-arg fn type: " ++ show t

  Let _ (NameBinding _ name) bindingExpr body letType -> do
    varDecl <- (AST.DeclarationStmt . AST.VarDecl)
               <$> (AST.VarDeclInitializer
                   <$> genIdentifier name
                   <*> genType (typeOf bindingExpr)
                   <*> genExpr bindingExpr)
    returnStmt <- returnSingle <$> genExpr body
    letType' <- genType letType
    let func = AST.Operand (AST.Literal (AST.FunctionLiteral (AST.FunctionSignature [] [letType']) (AST.Block [varDecl, returnStmt])))
    return (AST.Expression (AST.Application func []))

  Literal _ lit _ -> case lit of
    Int n      -> return $ literalExpr $ AST.BasicLiteral $ AST.IntLiteral n
    Bool True  -> return $ operandName (GI.Identifier "true")
    Bool False -> return $ operandName (GI.Identifier "false")
    String s   -> return $ literalExpr $ AST.BasicLiteral $ AST.StringLiteral $ AST.InterpretedStringLiteral s
    Unit{}     -> return emptyStructLiteral

  Tuple _ f s r t ->
    literalExpr <$> (AST.CompositeLiteral
                     <$> genType t
                     <*> (AST.LiteralValueElements . map AST.UnkeyedElement <$> mapM genExpr (f:s:r)))

  If _ condExpr thenExpr elseExpr exprType -> do
    condExpr' <- genExpr condExpr
    thenBranch <- (AST.Block . (:[]) . returnSingle) <$> genExpr thenExpr
    elseBranch <- (AST.ElseBlock . AST.Block . (:[]) . returnSingle) <$> genExpr elseExpr
    exprType' <- genType exprType

    let ifStmt = AST.IfStmt (AST.IfElse condExpr' thenBranch elseBranch)
    let func = AST.Operand (AST.Literal (AST.FunctionLiteral (AST.FunctionSignature [] [exprType']) (AST.Block [ifStmt])))
    return (AST.Expression (AST.Application func []))

  Slice _ exprs t -> do
    sliceType <- genType t
    elements <- AST.LiteralValueElements . map AST.UnkeyedElement <$> mapM genExpr exprs
    return (literalExpr (AST.CompositeLiteral sliceType elements))

  (Block _ [] _) -> return emptyStructLiteral

  (Block _ exprs t) -> do
    blockType <- genType t
    initStmts <- map (AST.SimpleStmt . AST.ExpressionStmt) <$> mapM genExpr (init exprs)
    returnStmt <- returnSingle <$> genExpr (last exprs)
    let func = AST.Operand (AST.Literal (AST.FunctionLiteral (AST.FunctionSignature [] [blockType]) (AST.Block $ initStmts ++ [returnStmt])))
    return (AST.Expression (AST.Application func []))

  RecordInitializer _ recordType values-> do
    elements <- AST.LiteralValueElements <$> mapM genField values
    structType <- genType recordType
    return (literalExpr (AST.CompositeLiteral structType elements))
    where
    genField (FieldInitializer _ label initializerExpr) =
      AST.KeyedElement <$> (AST.LiteralKeyName <$> genIdentifier label)
                       <*> genExpr initializerExpr

  RecordFieldAccess _ recordExpr name _ -> do
    primaryExpr <- genPrimaryExpression recordExpr
    AST.Expression . AST.Selector primaryExpr <$> genIdentifier name

  PackageMemberAccess _ pkgAlias name _ ->
    (AST.Expression . AST.Operand) <$> (AST.QualifiedOperandName <$> genIdentifier pkgAlias
                                                                 <*> genIdentifier name)

genBlock :: Expr Mono.Type -> Codegen AST.Block
genBlock expr = (AST.Block . (:[]) . AST.ReturnStmt . (:[])) <$> genExpr expr

genTopLevel :: Identifier -> Mono.Type -> Expr Mono.Type -> Codegen AST.TopLevelDeclaration
genTopLevel (Identifier "main") (Mono.TNoArgFn _ t) (NoArgFn _ body _) | isUniverseTypeConstructor "unit" t = do
  body' <- genExpr body
  let block = AST.Block [AST.SimpleStmt (AST.ExpressionStmt body')]
  return (AST.FunctionDecl (GI.Identifier "main") (AST.FunctionSignature [] []) block)
genTopLevel name _ (NoArgFn _ body (Mono.TNoArgFn _ returnType)) = do
  name' <- genIdentifier name
  returnType' <- genType returnType
  AST.FunctionDecl name' (AST.FunctionSignature [] [returnType']) <$> genBlock body
genTopLevel name (Mono.TFn _ paramType returnType) (Fn _ (NameBinding _ paramName) body _) = do
  name' <- genIdentifier name
  paramName' <- genIdentifier paramName
  paramType' <- genType paramType
  returnType' <- genType returnType
  AST.FunctionDecl name' (AST.FunctionSignature [AST.FunctionParameter paramName' paramType'] [returnType']) <$> genBlock body
genTopLevel name type' expr = do
   var <- AST.VarDeclInitializer <$> genIdentifier name
                                 <*> genType type'
                                 <*> genExpr expr
   return (AST.Decl (AST.VarDecl var))

genInstance :: InstantiatedDefinition -> Codegen AST.TopLevelDeclaration
genInstance (InstantiatedDefinition (Identifier _defName) _si name expr) =
  -- let comment = text "/*"
  --               $+$ text "Name:" <+> text defName
  --               $+$ text "Defined at:" <+> text (show $ getSourceInfo expr)
  --               $+$ text "Instantiated with type:" <+> pp (typeOf expr)
  --               $+$ text "Instantiated at:" <+> text (show $ unwrap si)
  --               $+$ text "*/"
  genTopLevel name (typeOf expr) expr

genMonomorphed :: MonomorphedDefinition -> Codegen AST.TopLevelDeclaration
genMonomorphed (MonomorphedDefinition _ name mt expr) =
  genTopLevel name mt expr

genImport :: ImportedPackage -> Codegen AST.ImportDecl
genImport (ImportedPackage _ identifier (Package (PackageDeclaration _ pkgName) _ _)) =
  AST.ImportDecl <$> genIdentifier identifier
                 <*> return (AST.InterpretedStringLiteral (intercalate "/" pkgName))

-- | Return the import alias name for the fmt package and possibly the code for
-- importing fmt (if not imported by the user).
getFmtImport :: [ImportedPackage] -> (GI.Identifier, Maybe AST.ImportDecl)
getFmtImport pkgs =
  case find isFmtPackage pkgs of
    Just (ImportedPackage _ (Identifier alias) _) -> (GI.Identifier alias, Nothing)
    Nothing ->
      let fmt = GI.Identifier "fmt"
      in (fmt, Just (AST.ImportDecl fmt (AST.InterpretedStringLiteral "fmt")))
  where
  isFmtPackage (ImportedPackage _ _ (Package (PackageDeclaration _ ["fmt"]) _ _)) = True
  isFmtPackage _ = False

prelude :: GI.Identifier -> [AST.TopLevelDeclaration]
prelude fmtAlias =
  let printSignature = AST.FunctionSignature [AST.FunctionParameter (GI.Identifier "x") (GT.Interface [])] []
      xOperand = AST.Expression (AST.Operand (AST.OperandName (GI.Identifier "x")))
      fmtApplication name = AST.Expression (AST.Application (AST.Operand (AST.QualifiedOperandName fmtAlias name)) (map AST.Argument [xOperand]))
  in [
    AST.FunctionDecl (GI.Identifier "print") printSignature (AST.Block [
        AST.SimpleStmt (AST.ExpressionStmt (fmtApplication (GI.Identifier "Print")))]),
    AST.FunctionDecl (GI.Identifier "println") printSignature (AST.Block [
        AST.SimpleStmt (AST.ExpressionStmt (fmtApplication (GI.Identifier "Println")))])
  ]

genPackage :: MonomorphedPackage -> Codegen AST.SourceFile
genPackage (MonomorphedPackage (PackageDeclaration _ name) imports is ms) = do
  imports' <- mapM genImport imports

  let (fmtAlias, fmtImport) = getFmtImport imports
      allImports = imports' ++ maybeToList fmtImport

  is' <- mapM genInstance (Set.toList is)
  ms' <- mapM genMonomorphed (Set.toList ms)

  let allTopLevel = prelude fmtAlias ++ is' ++ ms'

  return (AST.SourceFile (AST.PackageClause (safeName (last name))) allImports allTopLevel)

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
    where action = do path <- toFilePath backend name
                      code <- genPackage pkg
                      return [CompiledFile path (displayS (renderPretty 0.4 120 (pretty code)) "")]
