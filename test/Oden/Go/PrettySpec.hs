module Oden.Go.PrettySpec where

import           Oden.Go.Identifier
import           Oden.Go.AST
import           Oden.Go.Type
import           Oden.Go.Pretty ()

import           Text.PrettyPrint.Leijen

import           Test.Hspec

shouldPrintAs x s = displayS (renderPretty 0.4 100 (pretty x)) "" `shouldBe` s

identifierX = Expression (Operand (OperandName (Identifier "x")))

spec :: Spec
spec =
  describe "pretty" $ do

    describe "Type" $

      describe "Interface" $ do

        it "prints an empty interface" $
          Interface []
          `shouldPrintAs`
          "interface{}"

        it "prints a single-method interface" $
          Interface [Method (Identifier "foo") (Parameters [] False) (Returns [])]
          `shouldPrintAs`
          "interface {\n    foo()\n}"

    describe "PrimaryExpression" $ do

      it "prints a function application" $
        Application
        (Operand (OperandName (Identifier "f")))
        [Argument identifierX]
        `shouldPrintAs`
        "f(x)"

      it "prints a variadic function application" $
        Application
        (Operand (OperandName (Identifier "f")))
        [VariadicSliceArgument identifierX]
        `shouldPrintAs`
        "f(x...)"

    describe "Literal" $ do

      it "prints a function literal with no parameters and no return value" $
        FunctionLiteral
        (FunctionSignature
         []
         [])
        (Block [])
        `shouldPrintAs`
        "func () {}"

      it "prints a function literal with no parameters" $
        FunctionLiteral
        (FunctionSignature
         []
         [Basic (Identifier "int") False])
        (Block [ReturnStmt [identifierX]])
        `shouldPrintAs`
        "func () int {\n    return x\n}"

      it "prints a function literal with no return value" $
        FunctionLiteral
        (FunctionSignature
         [FunctionParameter (Identifier "_") (Basic (Identifier "int") False)]
         [])
        (Block [])
        `shouldPrintAs`
        "func (_ int) {}"

      it "prints a function literal" $
        FunctionLiteral
        (FunctionSignature
         [FunctionParameter (Identifier "x") (Basic (Identifier "int") False)]
         [Basic (Identifier "int") False])
        (Block [ReturnStmt [identifierX]])
        `shouldPrintAs`
        "func (x int) int {\n    return x\n}"

    describe "Declaration" $ do

      it "prints a type declaration" $
        TypeDecl (Identifier "Count") (Basic (Identifier "int") False)
        `shouldPrintAs`
        "type Count int"

      it "prints a struct type declaration" $
        TypeDecl
        (Identifier "Count")
        (Struct
         [StructField (Identifier "foo") (Basic (Identifier "int") False)])
        `shouldPrintAs`
        "type Count struct {\n    foo int\n}"

      it "prints an interface type declaration" $
        TypeDecl
        (Identifier "Count")
        (Interface
         [Method (Identifier "foo") (Parameters [] False) (Returns [])])
        `shouldPrintAs`
        "type Count interface {\n    foo()\n}"

      it "prints an const declaration" $
        ConstDecl
        (Identifier "PI")
        (Basic (Identifier "float") False)
        (Expression
         (Operand
         (Literal
           (BasicLiteral
           (FloatLiteral 3.14)))))
        `shouldPrintAs`
        "const PI float = 3.14"

      it "prints a var declaration" $
        VarDecl
        (VarDeclInitializer
         (Identifier "count")
         (Basic (Identifier "int") False)
         (Expression
          (Operand
          (Literal
            (BasicLiteral
            (IntLiteral 0))))))
        `shouldPrintAs`
        "var count int = 0"

      it "prints a function declaration" $
        FunctionDecl
        (Identifier "identity")
        (FunctionSignature
         [FunctionParameter (Identifier "x") (Basic (Identifier "int") False)]
         [Basic (Identifier "int") False])
        (Block [ReturnStmt [identifierX]])
        `shouldPrintAs`
        "func identity(x int) int {\n    return x\n}"

    describe "Block" $ do
      it "prints an empty block" $
        Block []
        `shouldPrintAs`
        "{}"

      it "indents a single stmt in a block" $
        Block [ReturnStmt []]
        `shouldPrintAs`
        "{\n    return\n}"

      it "indents multiple stmts in a block" $
        Block [ReturnStmt [], ReturnStmt []]
        `shouldPrintAs`
        "{\n    return\n    return\n}"

      it "indents nested blocks" $
        Block [BlockStmt (Block [ReturnStmt []])]
        `shouldPrintAs`
        "{\n    {\n        return\n    }\n}"

    describe "Stmt" $ do

      it "prints a return stmt without expressions" $
        ReturnStmt []
        `shouldPrintAs`
        "return"

      it "prints a return stmt with one expression" $
        ReturnStmt [identifierX]
        `shouldPrintAs`
        "return x"

      it "prints a return stmt with multiple expressions" $
        ReturnStmt [identifierX, identifierX]
        `shouldPrintAs`
        "return x, x"

    describe "SourceFile" $ do
      it "prints a source file with only a package clause" $
        SourceFile (PackageClause (Identifier "foo")) [] []
        `shouldPrintAs`
        "package foo\n"

      it "prints a source file with an import" $
        SourceFile
        (PackageClause (Identifier "foo"))
        [ImportDecl (Identifier "bar") (InterpretedStringLiteral "the/bar")]
        []
        `shouldPrintAs`
        "package foo\n\nimport bar \"the/bar\"\n"

      it "prints a source file with a function decl with no stmts" $
        SourceFile
        (PackageClause (Identifier "main"))
        []
        [FunctionDecl
         (Identifier "main")
         (FunctionSignature [] [])
         (Block [])]
        `shouldPrintAs`
        "package main\n\nfunc main() {}\n"

      it "prints a source file with a function decl with a return stmt" $
        SourceFile
        (PackageClause (Identifier "main"))
        []
        [FunctionDecl
         (Identifier "main")
         (FunctionSignature [] [])
         (Block [ReturnStmt []])]
        `shouldPrintAs`
        "package main\n\nfunc main() {\n    return\n}\n"

      it "prints a source file with an import and a function decl" $
        SourceFile
        (PackageClause (Identifier "main"))
        [ImportDecl (Identifier "bar") (InterpretedStringLiteral "the/bar")]
        [FunctionDecl
         (Identifier "main")
         (FunctionSignature [] [])
         (Block [])]
        `shouldPrintAs`
        "package main\n\nimport bar \"the/bar\"\n\nfunc main() {}\n"
