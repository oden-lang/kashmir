{-# LANGUAGE OverloadedStrings #-}
module Oden.ParserSpec where

import           Test.Hspec

import           Oden.Identifier
import           Oden.Parser
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Core.Operator

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<stdin>" l c)

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses qualified identifier" $
      parseExpr "foo.bar"
      `shouldSucceedWith`
      Symbol (src 1 1) (Qualified "foo" "bar")

    it "parses integer literal" $
      parseExpr "123"
      `shouldSucceedWith`
      Literal (src 1 1) (Int 123)

    it "parses false literal" $
      parseExpr "false"
      `shouldSucceedWith`
      Literal (src 1 1) (Bool False)

    it "parses true literal" $
      parseExpr "true"
      `shouldSucceedWith`
      Literal (src 1 1) (Bool True)

    it "parses string literal" $
      parseExpr "\"foo bar 123\""
      `shouldSucceedWith`
      Literal (src 1 1) (String "foo bar 123")

    it "parses fn expression" $
      parseExpr "fn x -> x"
      `shouldSucceedWith`
      Fn (src 1 1) [NameBinding (src 1 4) "x"] (Symbol (src 1 9) (Unqualified "x"))

    it "parses multi-arg fn expression" $
      parseExpr "fn x y z -> x"
      `shouldSucceedWith`
      Fn
      (src 1 1)
      [NameBinding (src 1 4) "x", NameBinding (src 1 6) "y", NameBinding (src 1 8) "z"]
      (Symbol (src 1 13) (Unqualified "x"))

    it "parses no-arg fn expression" $
      parseExpr "fn -> x"
      `shouldSucceedWith`
      Fn (src 1 1) [] (Symbol (src 1 7) (Unqualified "x"))

    it "parses if expression" $
      parseExpr "if a then b else c"
      `shouldSucceedWith`
      If
      (src 1 1)
      (Symbol (src 1 4) (Unqualified "a"))
      (Symbol (src 1 11) (Unqualified "b"))
      (Symbol (src 1 18) (Unqualified "c"))

    it "parses empty block as unit literal" $
      parseExpr "()"
      `shouldSucceedWith`
      Literal (src 1 1) Unit

    it "parses expression in parens" $
      parseExpr "(())"
      `shouldSucceedWith`
      Literal (src 1 2) Unit

    it "parses tuple with two elements" $
      parseExpr "(1, ())"
      `shouldSucceedWith`
      Tuple
      (src 1 1)
      (Literal (src 1 2) (Int 1))
      (Literal (src 1 5) Unit)
      []

    it "parses tuple with three elements" $
      parseExpr "(1, (), 2)"
      `shouldSucceedWith`
      Tuple
      (src 1 1)
      (Literal (src 1 2) (Int 1))
      (Literal (src 1 5) Unit)
      [Literal (src 1 9) (Int 2)]

    it "parses block of symbols" $
      parseExpr "{\n  x\n  y\n  z\n}"
      `shouldSucceedWith`
      Block (src 1 1) [
          Symbol (src 2 3) (Unqualified "x"),
          Symbol (src 3 3) (Unqualified "y"),
          Symbol (src 4 3) (Unqualified "z")
        ]

    it "parses let binding and block of symbol" $
      parseExpr "let x = y in { x }"
      `shouldSucceedWith`
      Let
      (src 1 1)
      [LetPair (src 1 5) (NameBinding (src 1 5) "x") (Symbol (src 1 9) (Unqualified "y"))]
      (Block (src 1 14) [Symbol (src 1 16) (Unqualified "x")])

    it "parses block with let binding and block of symbol" $
      parseExpr "{\n  let x = y in {\n    x\n  }\n}"
      `shouldSucceedWith`
      Block (src 1 1) [
          Let
          (src 2 3)
          [LetPair (src 2 7) (NameBinding (src 2 7) "x") (Symbol (src 2 11) (Unqualified "y"))]
          (Block (src 2 16) [Symbol (src 3 5) (Unqualified "x")])
        ]

    it "fails on if expression with newlines" pending
    it "fails on let expression with newlines" pending

    it "parses let expression" $
      parseExpr "let x = y in z"
      `shouldSucceedWith`
      Let
      (src 1 1)
      [LetPair (src 1 5) (NameBinding (src 1 5) "x") (Symbol (src 1 9) (Unqualified "y"))]
      (Symbol (src 1 14) (Unqualified "z"))

    it "parses unary negative operator application" $
      parseExpr "-x"
      `shouldSucceedWith`
      UnaryOp
      (src 1 1)
      Negative
      (Symbol (src 1 2) (Unqualified "x"))

    it "parses unary positve operator application" $
      parseExpr "+ x"
      `shouldSucceedWith`
      UnaryOp
      (src 1 1)
      Positive
      (Symbol (src 1 3) (Unqualified "x"))

    it "parses binary operator application" $
      parseExpr "x + y"
      `shouldSucceedWith`
      BinaryOp
      (src 1 3)
      Add
      (Symbol (src 1 1) (Unqualified "x"))
      (Symbol (src 1 5) (Unqualified "y"))

    it "parses string concat application" $
      parseExpr "x ++ y"
      `shouldSucceedWith`
      BinaryOp
      (src 1 3)
      Concat
      (Symbol (src 1 1) (Unqualified "x"))
      (Symbol (src 1 6) (Unqualified "y"))

    it "parses single-arg fn application" $
      parseExpr "x(y)"
      `shouldSucceedWith`
      Application
      (src 1 1)
      (Symbol (src 1 1) (Unqualified "x"))
      [Symbol (src 1 3) (Unqualified "y")]

    it "parses single-arg fn application" $
      parseExpr "(fn x -> x)(y)"
      `shouldSucceedWith`
      Application
      (src 1 1)
      (Fn (src 1 2) [NameBinding (src 1 5) "x"] (Symbol (src 1 10) (Unqualified "x")))
      [Symbol (src 1 13) (Unqualified "y")]

    it "ignores whitespace" $
      parseExpr "x(   \n\n y \r\n\t   )"
      `shouldSucceedWith`
      Application
      (src 1 1)
      (Symbol (src 1 1) (Unqualified "x"))
      [Symbol (src 3 2) (Unqualified "y")]

    it "ignores comments" $
      parseExpr "\n// foobar\nx"
      `shouldSucceedWith`
      Symbol (src 3 1) (Unqualified "x")

    it "ignores multi-line comments" $
      parseExpr "/*\n\n foo //whatever\tbar\n*/x"
      `shouldSucceedWith`
      Symbol (src 4 3) (Unqualified "x")

    it "parses slice literal" $
      parseExpr "[]{x, y, z}"
      `shouldSucceedWith`
      Slice (src 1 1) [
          Symbol (src 1 4) (Unqualified "x"),
          Symbol (src 1 7) (Unqualified "y"),
          Symbol (src 1 10) (Unqualified "z")
        ]

    it "parses subscript" $
      parseExpr "a[b]"
      `shouldSucceedWith`
      Subscript (src 1 1)
        (Symbol (src 1 1) (Unqualified "a"))
        [(Singular (Symbol (src 1 3) (Unqualified "b")))]

    it "parses closed sublices" $
      parseExpr "a[b:c]"
      `shouldSucceedWith`
      Subscript (src 1 1)
        (Symbol (src 1 1) (Unqualified "a"))
        [(Range (Symbol (src 1 3) (Unqualified "b"))
                (Symbol (src 1 5) (Unqualified "c")))]

    it "parses subslices with open start" $
      parseExpr "a[:c]"
      `shouldSucceedWith`
      Subscript (src 1 1)
        (Symbol (src 1 1) (Unqualified "a"))
        [OpenStart (Symbol (src 1 4) (Unqualified "c"))]

    it "parses subslices with open ending" $
      parseExpr "a[b:]"
      `shouldSucceedWith`
      Subscript (src 1 1)
        (Symbol (src 1 1) (Unqualified "a"))
        [OpenEnd (Symbol (src 1 3) (Unqualified "b"))]

    it "fails on subslices with open start and end" $
      shouldFail $ parseExpr "a[:]"


  describe "parseTopLevel" $ do
    it "parses type signature" $
      parseTopLevel "x :: int"
      `shouldSucceedWith`
      TypeSignature (src 1 1) "x" (Implicit (src 1 6) (TEBasic (src 1 6) TEInt))

    it "parses type signature without explicit forall" $
      parseTopLevel "x :: int -> int"
      `shouldSucceedWith`
      TypeSignature
      (src 1 1)
      "x"
      (Implicit (src 1 6) (TEFn (src 1 6) (TEBasic (src 1 6) TEInt) [TEBasic (src 1 13) TEInt]))

    it "parses type signature with no-arg fn" $
      parseTopLevel "x :: -> ()"
      `shouldSucceedWith`
      TypeSignature
      (src 1 1)
      "x"
      (Implicit (src 1 6) (TENoArgFn (src 1 6) (TEUnit (src 1 9))))

    it "parses type signature with int slice" $
      parseTopLevel "x :: []{int}"
      `shouldSucceedWith`
      TypeSignature
      (src 1 1)
      "x"
      (Implicit (src 1 6) (TESlice (src 1 6) (TEBasic (src 1 9) TEInt)))

    it "parses type signature with string slice" $
      parseTopLevel "x :: []{string}"
      `shouldSucceedWith`
      TypeSignature
      (src 1 1)
      "x"
      (Implicit (src 1 6) (TESlice (src 1 6) (TEBasic (src 1 9) TEString)))


    it "parses polymorphic type signature with implicit forall" $
      parseTopLevel "x :: #a -> #a"
      `shouldSucceedWith`
      TypeSignature (src 1 1) "x" (Implicit (src 1 6) (TEFn (src 1 6) (TEVar (src 1 6) "a") [TEVar (src 1 12) "a"]))

    it "parses polymorphic type signature with explicit forall" $
      parseTopLevel "x :: forall #a. #a -> #a"
      `shouldSucceedWith`
      TypeSignature
      (src 1 1)
      "x"
      (Explicit
       (src 1 6)
       [TVarBindingExpr (src 1 13) "a"]
       (TEFn (src 1 17) (TEVar (src 1 17) "a") [TEVar (src 1 23) "a"]))

    -- TODO: would be nice...
    --
    -- it "parses type alias" $
    --   parseTopLevel "type x(#a) = #a -> #a"
    --   `shouldSucceedWith`
    --   TypeAlias (src 1 1) "x" [TVarBindingExpr (src 1 8) "a"] (TEFn (src 1 14) (TEVar (src 1 14) "a") [TEVar (src 1 20) "a"])

    -- it "parses type with nested type constructor" $
    --   parseTopLevel "type x(#a) = f(#a, g(#a, #a))"
    --   `shouldSucceedWith`
    --   TypeAlias (src 1 1) "x" [TVarBindingExpr (src 1 8) "a"] (TECon (src 1 14) "f" [TEVar (src 1 16) "a", TECon (src 1 20) "g" [TEVar (src 1 22) "a", TEVar (src 1 26) "a"]])

    it "parses value definition" $
      parseTopLevel "x = y"
      `shouldSucceedWith`
      ValueDefinition (src 1 1) "x" (Symbol (src 1 5) (Unqualified "y"))

    it "parses fn definition" $
      parseTopLevel "f = fn x -> x"
      `shouldSucceedWith`
      ValueDefinition (src 1 1) "f" (Fn (src 1 5) [NameBinding (src 1 8) "x"] (Symbol (src 1 13) (Unqualified "x")))

    it "parses short-hand fn definition" $
      parseTopLevel "f x -> x"
      `shouldSucceedWith`
      FnDefinition (src 1 1) "f" [NameBinding (src 1 3) "x"] (Symbol (src 1 8) (Unqualified "x"))

    it "parses short-hand no-arg fn definition" $
      parseTopLevel "side-effect -> x"
      `shouldSucceedWith`
      FnDefinition (src 1 1) "side-effect" [] (Symbol (src 1 16) (Unqualified "x"))

    it "parses short-hand multi-arg fn definition" $
      parseTopLevel "f x y z -> x"
      `shouldSucceedWith`
      FnDefinition (src 1 1) "f"
      [NameBinding (src 1 3) "x", NameBinding (src 1 5) "y", NameBinding (src 1 7) "z"]
      (Symbol (src 1 12) (Unqualified "x"))

  describe "parsePackage" $ do
    it "parses package declaration" $
      parsePackage "<stdin>" "package foo/bar"
      `shouldSucceedWith`
      Package (PackageDeclaration (src 1 1) ["foo", "bar"]) []

    it "parses imports" $
      parsePackage "<stdin>" "package foo/bar\nimport bar/baz\nimport foo\nimport github.com/foo/bar"
      `shouldSucceedWith`
      Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [
          ImportDeclaration (src 2 1) ["bar", "baz"],
          ImportDeclaration (src 3 1) ["foo"],
          ImportDeclaration (src 4 1) ["github.com", "foo", "bar"]
        ]

    it "parses github.com import" $
      parsePackage "<stdin>" "package foo/bar\nimport github.com/foo/bar"
      `shouldSucceedWith`
      Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [ImportDeclaration (src 2 1) ["github.com", "foo", "bar"]]

    it "parses definitions" $
      parsePackage "<stdin>" "package foo/bar\nimport bar/baz"
      `shouldSucceedWith`
      Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [ImportDeclaration (src 2 1) ["bar", "baz"]]
