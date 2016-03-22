module Oden.InferSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Core.Operator
import           Oden.Environment
import           Oden.Identifier
import           Oden.Infer            (inferExpr)
import           Oden.Infer.Environment
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Oden.Assertions
import           Oden.Infer.Fixtures

spec :: Spec
spec = describe "inferExpr" $ do
  it "infers int literal" $
    inferExpr empty (uLiteral (uInt 1))
    `shouldSucceedWith`
    (forall [] typeInt,
     tLiteral (tInt 1) typeInt)

  it "infers int slice" $
    inferExpr empty (uSlice [uLiteral (uInt 1)])
    `shouldSucceedWith`
    (forall [] intSlice,
     tSlice [tLiteral (tInt 1) typeInt] intSlice)

  it "fails on mixed type slice" $
    shouldFail $
      inferExpr empty (uSlice [uLiteral (uInt 1),
                                      uLiteral (uString "foo")])

  it "infers tuple" $
    let tupleType = TTuple missing typeInt typeString [typeUnit]
    in
      inferExpr empty (uTuple (uLiteral (uInt 1))
                                     (uLiteral (uString "foo"))
                                     [uLiteral uUnit])
      `shouldSucceedWith`
      (forall [] tupleType,
      tTuple
      (tLiteral (tInt 1) typeInt)
      (tLiteral (tString "foo") typeString)
      [tLiteral tUnit typeUnit]
      tupleType)

  it "infers struct member access" $
    inferExpr empty (uFn
                     (uNameBinding (Identifier "x"))
                     (uMemberAccess
                      (uSymbol (Identifier "x"))
                      (Identifier "y")))
    `shouldSucceedWith`
    let structType = TRecord missing (RExtension missing (Identifier "y") tvarA tvarB) in
      (forall [tvarBinding tvA, tvarBinding tvB] (typeFn structType tvarA),
      tFn
      (tNameBinding (Identifier "x"))
      (tFieldAccess (tSymbol (Identifier "x") structType) (Identifier "y") tvarA)
      (typeFn structType tvarA))

  it "infers package member access" $
    inferExpr fooBarPkgEnv (uMemberAccess (uSymbol (Identifier "foo")) (Identifier "Bar"))
    `shouldSucceedWith`
    (forall [] typeInt,
     tPackageMemberAcccess (Identifier "foo") (Identifier "Bar") typeInt)

  it "infers identity fn" $
    inferExpr empty (uFn (uNameBinding (Identifier "x")) (uSymbol (Identifier "x")))
    `shouldSucceedWith`
    (forall [tvarBinding tvA] (typeFn tvarA tvarA),
     tFn (tNameBinding (Identifier "x")) (tSymbol (Identifier "x") tvarA) (typeFn tvarA tvarA))

  it "infers no-arg fn" $
    inferExpr empty (uNoArgFn (uLiteral (uBool True)))
    `shouldSucceedWith`
    (forall [] (typeNoArgFn typeBool),
     tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool))

  it "infers no-arg fn application" $
    inferExpr empty (uApplication (uNoArgFn (uLiteral (uBool True))) [])
    `shouldSucceedWith`
    (forall [] typeBool,
     tNoArgApplication (tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool)) typeBool)

  it "infers multi-arg fn application" $
    inferExpr empty (uApplication
                     (uFn (uNameBinding (Identifier "x")) (uFn (uNameBinding (Identifier "y")) (uLiteral (uInt 1))))
                     [uLiteral (uBool False), uLiteral (uBool False)])
    `shouldSucceedWith`
    (forall [] typeInt,
     tApplication
     (tApplication
      (tFn (tNameBinding (Identifier "x"))
       (tFn (tNameBinding (Identifier "y")) (tLiteral (tInt 1) typeInt) (typeBool `typeFn` typeInt))
       (typeBool `typeFn` (typeBool `typeFn` typeInt)))
      (tLiteral (tBool False) typeBool)
      (typeBool `typeFn` typeInt))
     (tLiteral (tBool False) typeBool)
     typeInt)

  it "fails in fn application with type mismatch" $
    shouldFail $
      inferExpr predefAndStringLength (uApplication
                                       (uSymbol (Identifier "stringLength"))
                                       [uLiteral (uInt 1)])

  it "infers nested fn application" $
    inferExpr
    predef
    (uOp
     Or
     (uOp
      And
      (uLiteral (uBool False))
      (uLiteral (uBool False)))
     (uLiteral (uBool True)))
    `shouldSucceedWith`
    (forall [] typeBool,
     tOp
     Or
     (tOp
      And
      (tLiteral (tBool False) typeBool)
      (tLiteral (tBool False) typeBool)
      typeBool)
     (tLiteral (tBool True) typeBool)
     typeBool)

  it "infers fn application with any-type" $
    inferExpr
      predefAndIdentityAny
      (uApplication
       (uSymbol (Identifier "identity"))
       [uLiteral (uBool False)])
    `shouldSucceedWith`
    (forall [] typeAny,
     tUncurriedFnApplication
      (tSymbol (Identifier "identity") (typeUncurried [typeAny] [typeAny]))
      [tLiteral (tBool False) typeBool]
      typeAny)

  it "infers 1 + 1" $
    inferExpr
      predef
      (uOp
       Add
       (uLiteral (uInt 1))
       (uLiteral (uInt 1)))
    `shouldSucceedWith`
    (forall [] typeInt,
      tOp
      Add
      (tLiteral (tInt 1) typeInt)
      (tLiteral (tInt 1) typeInt)
      typeInt)

  it "infers fn application with any-type with multiple \"instances\"" $
    inferExpr
      predefAndIdentityAny
      (uApplication
       (uSymbol (Identifier "identity"))
       [uApplication
        (uSymbol (Identifier "identity"))
        [uLiteral (uBool False)]])
    `shouldSucceedWith`
    (forall [] typeAny,
     tUncurriedFnApplication
      (tSymbol (Identifier "identity") (typeUncurried [typeAny] [typeAny]))
      [tUncurriedFnApplication
       (tSymbol (Identifier "identity") (typeUncurried [typeAny] [typeAny]))
       [tLiteral (tBool False) typeBool]
       typeAny]
      typeAny)

  it "infers let" $
    inferExpr empty (uLet (uNameBinding (Identifier "x")) (uLiteral (uInt 1)) (uSymbol (Identifier "x")))
    `shouldSucceedWith`
    (forall [] typeInt,
     tLet (tNameBinding (Identifier "x")) (tLiteral (tInt 1) typeInt) (tSymbol (Identifier "x") typeInt) typeInt)

  it "infers let with shadowing" $
    inferExpr empty (uLet
                     (uNameBinding (Identifier "x"))
                     (uLiteral (uInt 1))
                     (uLet
                      (uNameBinding (Identifier "x"))
                      (uSymbol (Identifier "x"))
                      (uSymbol (Identifier "x"))))
    `shouldSucceedWith`
    (forall [] typeInt,
     tLet
      (tNameBinding (Identifier "x"))
      (tLiteral (tInt 1) typeInt)
      (tLet
       (tNameBinding (Identifier "x"))
       (tSymbol (Identifier "x") typeInt)
       (tSymbol (Identifier "x") typeInt)
       typeInt)
      typeInt)

  it "infers polymorphic if" $
    inferExpr empty (uFn (uNameBinding (Identifier "x")) (uIf (uLiteral (uBool True)) (uSymbol (Identifier "x")) (uSymbol (Identifier "x"))))
    `shouldSucceedWith`
    (forall [tvarBinding tvA] (typeFn tvarA tvarA),
     tFn (tNameBinding (Identifier "x")) (tIf (tLiteral (tBool True) typeBool)
                          (tSymbol (Identifier "x") tvarA)
                          (tSymbol (Identifier "x") tvarA)
                          tvarA) (typeFn tvarA tvarA))

  it "infers single-arg uncurried func application" $
    inferExpr predef (uApplication (uSymbol (Identifier "len")) [uSlice [uLiteral (uBool True)]])
    `shouldSucceedWith`
    (forall [] typeInt,
     tUncurriedFnApplication (Core.Symbol missing (Identifier "len") (TUncurriedFn missing [TSlice predefined typeBool] [typeInt]))
                            [Core.Slice missing [Core.Literal missing (tBool True) typeBool] (typeSlice typeBool)]
     typeInt)

  it "infers single-arg uncurried func application" $
    inferExpr predefAndMax (uApplication (uSymbol (Identifier "max"))
                                                [uLiteral (uInt 0)
                                                ,uLiteral (uInt 1)])
    `shouldSucceedWith`
    (forall [] typeInt,
     tUncurriedFnApplication (tSymbol (Identifier "max") (typeUncurried [typeInt, typeInt] [typeInt]))
                            [tLiteral (tInt 0) typeInt
                            ,tLiteral (tInt 1) typeInt]
     typeInt)

  it "infers variadic func application" $
    inferExpr predefAndMaxVariadic (uApplication (uSymbol (Identifier "max"))
                                                        [uLiteral (uInt 0)
                                                        ,uLiteral (uInt 1)])
    `shouldSucceedWith`
    (forall [] typeInt,
     tUncurriedFnApplication (tSymbol (Identifier "max") (typeVariadic [] typeInt [typeInt]))
                            [tSlice [tLiteral (tInt 0) typeInt
                                        ,tLiteral (tInt 1) typeInt] typeInt]
     typeInt)

  it "infers variadic no-arg func application" $
    inferExpr predefAndMaxVariadic (uApplication (uSymbol (Identifier "max")) [])
    `shouldSucceedWith`
    (forall [] typeInt,
     tUncurriedFnApplication (tSymbol (Identifier "max") (typeVariadic [] typeInt [typeInt]))
                            [tSlice [] typeInt]
     typeInt)

  it "infers record initializer" $
    let structType = TRecord missing (RExtension missing (Identifier "msg") typeString (REmpty missing)) in
      inferExpr predef (Untyped.RecordInitializer
                        missing
                        [Untyped.FieldInitializer missing (Identifier "msg") (Untyped.Literal missing (Untyped.String "hello"))])
      `shouldSucceedWith`
      (forall [] structType,
       Core.RecordInitializer missing structType [Core.FieldInitializer missing (Identifier "msg") (Core.Literal missing (Core.String "hello") typeString)])

  it "infers record field access fn" $
    let recordType = typeRecord (rowExt (Identifier "a") tvarA tvarB)
        functionType = typeFn recordType tvarA in
    inferExpr empty (uFn (uNameBinding (Identifier "x")) (uMemberAccess (uSymbol (Identifier "x")) (Identifier "a")))
    `shouldSucceedWith`
    (forall [tvarBinding tvA, tvarBinding tvB] functionType,
     tFn
     (tNameBinding (Identifier "x"))
     (tFieldAccess (tSymbol (Identifier "x") recordType) (Identifier "a") tvarA)
     functionType)
