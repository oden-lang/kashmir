module Oden.Backend.GoBackendSpec where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Set                  as Set hiding (map)

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Core
import           Oden.Compiler.Monomorphization
import qualified Oden.Go.Identifier as GI
import qualified Oden.Go.AST as AST
import qualified Oden.Go.Type as GT
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Monomorphic

import           Test.Hspec

import           Oden.Assertions

missing = Metadata Missing

gen :: MonomorphedPackage -> Either CodegenError AST.SourceFile
gen pkg = runExcept (runReaderT (genPackage pkg) pkg)

identifierExpr s = AST.Expression (AST.Operand (AST.OperandName (GI.Identifier s)))

mainPkg = PackageDeclaration missing ["main"]
fmtImport = AST.ImportDecl (GI.Identifier "fmt") (AST.InterpretedStringLiteral "fmt")

typeUnit = TCon missing (nameInUniverse "unit")

mainFn :: Expr Type -> MonomorphedDefinition
mainFn expr =
  MonomorphedDefinition
  missing
  (Identifier "main")
  (TNoArgFn missing typeUnit)
  (NoArgFn missing expr (TNoArgFn missing typeUnit))

literalExpr :: AST.Literal -> AST.Expression
literalExpr = AST.Expression . AST.Operand . AST.Literal

emptyStructLiteral :: AST.Expression
emptyStructLiteral =
  literalExpr
  (AST.CompositeLiteral
    (GT.Struct [])
    (AST.LiteralValueElements []))

spec :: Spec
spec =
  describe "genPackage" $ do
    it "includes the prelude" $
      gen (MonomorphedPackage mainPkg [] Set.empty Set.empty)
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt"))

    it "translates an empty block for the main function" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (mainFn (Block missing [] typeUnit))))
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt") ++ [
        AST.FunctionDecl (GI.Identifier "main") (AST.FunctionSignature [] []) (AST.Block [])
       ])

    it "returns an empty struct for an empty block in non-main functions" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (MonomorphedDefinition
                           missing
                           (Identifier "foo")
                           (TNoArgFn missing typeUnit)
                           (NoArgFn
                            missing
                            (Block missing [] typeUnit)
                            (TNoArgFn missing typeUnit)))))
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt") ++ [
        AST.FunctionDecl
        (GI.Identifier "foo")
        (AST.FunctionSignature [] [GT.Struct []])
        (AST.Block [AST.ReturnStmt [emptyStructLiteral]])
       ])
