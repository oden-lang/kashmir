module Oden.InferSpec where

import           Test.Hspec

import qualified Oden.Core             as Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Core.Operator
import           Oden.Environment
import           Oden.Identifier
import qualified Oden.Infer            as Infer
import           Oden.Infer            (inferExpr)
import           Oden.Infer.Environment
import           Oden.Predefined
import           Oden.SourceInfo
import           Oden.Type.Basic
import           Oden.Type.Polymorphic
import           Oden.Type.Signature

import           Oden.Assertions

inferDefinition :: TypingEnvironment -> Untyped.Definition -> Either Infer.TypeError Core.Definition
inferDefinition env def = snd <$> Infer.inferDefinition env def

typeAny = TAny Missing
typeUnit = TUnit Missing
typeInt = TBasic Missing TInt
typeBool = TBasic Missing TBool
typeString = TBasic Missing TString

tvA :: TVar
tvA = TV "a"

tvarA :: Type
tvarA = TVar Missing (TV "a")

typeSlice = TSlice Missing
intSlice = typeSlice typeInt

typeFn = TFn Missing
typeNoArgFn = TNoArgFn Missing
typeUncurried = TUncurriedFn Missing
typeVariadic = TVariadicFn Missing

tsUnit = TSUnit Missing
tsVar = TSVar Missing
tsSymbol = TSSymbol Missing
tsFn = TSFn Missing

implicit = Implicit Missing
explicit = Explicit Missing
varBinding = SignatureVarBinding Missing

forall = Forall Missing
tvarBinding = TVarBinding Missing

uSymbol                 = Untyped.Symbol Missing
uOp                     = Untyped.BinaryOp Missing
uApplication            = Untyped.Application Missing
uFn                     = Untyped.Fn Missing
uNoArgFn                = Untyped.NoArgFn Missing
uLet                    = Untyped.Let Missing
uLiteral                = Untyped.Literal Missing
uTuple                  = Untyped.Tuple Missing
uIf                     = Untyped.If Missing
uSlice                  = Untyped.Slice Missing
uBlock                  = Untyped.Block Missing

uInt    = Untyped.Int
uString = Untyped.String
uBool   = Untyped.Bool
uUnit   = Untyped.Unit

uNameBinding = Untyped.NameBinding Missing
uDefinition = Untyped.Definition Missing

tSymbol                 = Core.Symbol Missing
tOp                     = Core.BinaryOp Missing
tApplication            = Core.Application Missing
tNoArgApplication       = Core.NoArgApplication Missing
tUncurriedFnApplication = Core.UncurriedFnApplication Missing
tFn                     = Core.Fn Missing
tNoArgFn                = Core.NoArgFn Missing
tLet                    = Core.Let Missing
tLiteral                = Core.Literal Missing
tTuple                  = Core.Tuple Missing
tIf                     = Core.If Missing
tSlice                  = Core.Slice Missing
tBlock                  = Core.Block Missing

tUnit   = Core.Unit
tInt    = Core.Int
tString = Core.String
tBool   = Core.Bool

tDefinition = Core.Definition Missing
tNameBinding = Core.NameBinding Missing

predef :: TypingEnvironment
predef = fromDefinitions predefined

predefAndStringLength :: TypingEnvironment
predefAndStringLength =  predef `extend` ("stringLength",
                                          Local Predefined "stringLength" $ forall [] (typeFn typeString typeInt))

predefAndMax :: TypingEnvironment
predefAndMax =  predef `extend` ("max",
                                 Local Predefined "max" $ forall [] (typeUncurried [typeInt, typeInt] [typeInt]))

predefAndPair :: TypingEnvironment
predefAndPair = predef `extend` ("pair",
                                 Local Predefined "pair" $ forall [] (typeUncurried [] [typeInt, typeString]))

predefAndMaxVariadic :: TypingEnvironment
predefAndMaxVariadic = predef `extend` ("max",
                                        Local Predefined "max" $ forall [] (typeVariadic [] typeInt typeInt))

predefAndIdentityAny :: TypingEnvironment
predefAndIdentityAny = predef `extend` ("identity",
                                        Local Predefined "identity" $ forall [] (typeUncurried [typeAny] ([typeAny])))

booleanOp :: Type
booleanOp = typeFn typeBool (typeFn typeBool typeBool)

countToZero :: Untyped.Expr
countToZero =
  uFn
  (uNameBinding "x")
  (uIf
   (uOp
    Equals
    (uSymbol (Unqualified "x"))
    (uLiteral (uInt 0)))
   (uLiteral (uInt 0))
   (uApplication
    (uSymbol (Unqualified "f"))
    [uOp
     Subtract
     (uSymbol (Unqualified "x"))
     (uLiteral (uInt 1))]))

intToInt :: Type
intToInt = typeFn typeInt typeInt

intToIntToInt :: Type
intToIntToInt = typeFn typeInt (typeFn typeInt typeInt)

countToZeroTyped :: Core.Definition
countToZeroTyped =
  tDefinition
   "f"
   (forall [] (typeFn typeInt typeInt),
    tFn
    (tNameBinding "x")
    (tIf
     (tOp
      Equals
      (tSymbol (Unqualified "x") typeInt)
      (tLiteral (tInt 0) typeInt)
      typeBool)
     (tLiteral (tInt 0) typeInt)
     (tApplication
      (tSymbol (Unqualified "f") intToInt)
      (tOp
       Subtract
       (tSymbol (Unqualified "x") typeInt)
       (tLiteral (tInt 1) typeInt)
       typeInt)
      typeInt)
     typeInt)
    intToInt)

twiceUntyped :: Untyped.Expr
twiceUntyped =
  uFn
  (uNameBinding "f")
  (uFn
   (uNameBinding "x")
   (uApplication
     (uSymbol (Unqualified "f"))
     [uApplication
     (uSymbol (Unqualified "f"))
     [uSymbol (Unqualified "x")]]))

twiceTyped :: Core.Definition
twiceTyped =
  tDefinition "twice" (forall [tvarBinding tvA] (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)),
                           tFn
                           (tNameBinding "f")
                           (tFn
                           (tNameBinding "x")
                           (tApplication
                             (tSymbol (Unqualified "f") (typeFn tvarA tvarA))
                             (tApplication
                             (tSymbol (Unqualified "f") (typeFn tvarA tvarA))
                             (tSymbol (Unqualified "x") tvarA)
                             tvarA)
                             tvarA)
                           (typeFn tvarA tvarA))
                           (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)))

spec :: Spec
spec = do
  describe "inferExpr" $ do
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
      let tupleType = (TTuple Missing typeInt typeString [typeUnit])
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

    it "infers identity fn" $
      inferExpr empty (uFn (uNameBinding "x") (uSymbol (Unqualified "x")))
      `shouldSucceedWith`
      (forall [tvarBinding tvA] (typeFn tvarA tvarA),
       tFn (tNameBinding "x") (tSymbol (Unqualified "x") tvarA) (typeFn tvarA tvarA))

    it "infers no-arg fn" $
      inferExpr empty (uNoArgFn (uLiteral (uBool True)))
      `shouldSucceedWith`
      (forall [] (typeNoArgFn typeBool),
       tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool))

    it "infers no-arg fn application" $
      inferExpr empty (uApplication (uNoArgFn (uLiteral (uBool True))) [])
      `shouldSucceedWith`
      (forall [] typeBool,
       (tNoArgApplication (tNoArgFn (tLiteral (tBool True) typeBool) (typeNoArgFn typeBool))) typeBool)

    it "infers multi-arg fn application" $
      inferExpr empty (uApplication
                       (uFn (uNameBinding "x") (uFn (uNameBinding "y") (uLiteral (uInt 1))))
                       [uLiteral (uBool False), uLiteral (uBool False)])
      `shouldSucceedWith`
      (forall [] typeInt,
       (tApplication
        (tApplication
         (tFn (tNameBinding "x")
          (tFn (tNameBinding "y") (tLiteral (tInt 1) typeInt) (typeBool `typeFn` typeInt))
          (typeBool `typeFn` (typeBool `typeFn` typeInt)))
         (tLiteral (tBool False) typeBool)
         (typeBool `typeFn` typeInt))
        (tLiteral (tBool False) typeBool)
        typeInt))

    it "fails in fn application with type mismatch" $
      shouldFail $
        inferExpr predefAndStringLength (uApplication
                                         (uSymbol (Unqualified "stringLength"))
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
         (uSymbol (Unqualified "identity"))
         [uLiteral (uBool False)])
      `shouldSucceedWith`
      (forall [] typeAny,
       tUncurriedFnApplication
        (tSymbol (Unqualified "identity") (typeUncurried [typeAny] [typeAny]))
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
         (uSymbol (Unqualified "identity"))
         [uApplication
          (uSymbol (Unqualified "identity"))
          [uLiteral (uBool False)]])
      `shouldSucceedWith`
      (forall [] typeAny,
       tUncurriedFnApplication
        (tSymbol (Unqualified "identity") (typeUncurried [typeAny] [typeAny]))
        [tUncurriedFnApplication
         (tSymbol (Unqualified "identity") (typeUncurried [typeAny] [typeAny]))
         [tLiteral (tBool False) typeBool]
         typeAny]
        typeAny)

    it "infers let" $
      inferExpr empty (uLet (uNameBinding "x") (uLiteral (uInt 1)) (uSymbol (Unqualified "x")))
      `shouldSucceedWith`
      (forall [] typeInt,
       tLet (tNameBinding "x") (tLiteral (tInt 1) typeInt) (tSymbol (Unqualified "x") typeInt) typeInt)

    it "infers let with shadowing" $
      inferExpr empty (uLet
                       (uNameBinding "x")
                       (uLiteral (uInt 1))
                       (uLet
                        (uNameBinding "x")
                        (uSymbol (Unqualified "x"))
                        (uSymbol (Unqualified "x"))))
      `shouldSucceedWith`
      (forall [] typeInt,
       tLet
        (tNameBinding "x")
        (tLiteral (tInt 1) typeInt)
        (tLet
         (tNameBinding "x")
         (tSymbol (Unqualified "x") typeInt)
         (tSymbol (Unqualified "x") typeInt)
         typeInt)
        typeInt)

    it "infers polymorphic if" $
      inferExpr empty (uFn (uNameBinding "x") (uIf (uLiteral (uBool True)) (uSymbol (Unqualified "x")) (uSymbol (Unqualified "x"))))
      `shouldSucceedWith`
      (forall [tvarBinding tvA] (typeFn tvarA tvarA),
       tFn (tNameBinding "x") (tIf (tLiteral (tBool True) typeBool)
                            (tSymbol (Unqualified "x") tvarA)
                            (tSymbol (Unqualified "x") tvarA)
                            tvarA) (typeFn tvarA tvarA))

    it "infers single-arg uncurried func application" $
      inferExpr predef (uApplication (uSymbol (Unqualified "len")) [uSlice [uLiteral (uBool True)]])
      `shouldSucceedWith`
      (forall [] (TBasic Predefined TInt),
       tUncurriedFnApplication (Core.Symbol Missing (Unqualified "len") (TUncurriedFn Missing [TSlice Predefined (TBasic Missing TBool)] [TBasic Predefined TInt]))
                              [Core.Slice Missing [Core.Literal Missing (tBool True) typeBool] (typeSlice typeBool)]
       (TBasic Predefined TInt))

    it "infers two arg uncurried func application" $
      inferExpr predefAndMax (uApplication (uSymbol (Unqualified "max"))
                                                  [uLiteral (uInt 0)
                                                  ,uLiteral (uInt 1)])
      `shouldSucceedWith`
      (forall [] typeInt,
       tUncurriedFnApplication (tSymbol (Unqualified "max") (typeUncurried [typeInt, typeInt] [typeInt]))
                              [tLiteral (tInt 0) typeInt
                              ,tLiteral (tInt 1) typeInt]
       typeInt)

    it "infers no-arg uncurried multi-value func application" $
      inferExpr predefAndPair (uApplication (uSymbol (Unqualified "pair")) [])
      `shouldSucceedWith`
      (forall [] (TTuple Missing typeInt typeString []),
       tUncurriedFnApplication (tSymbol (Unqualified "pair") (typeUncurried [] [typeInt, typeString])) []
       (TTuple Missing typeInt typeString []))

    it "infers variadic func application" $
      inferExpr predefAndMaxVariadic (uApplication (uSymbol (Unqualified "max"))
                                                          [uLiteral (uInt 0)
                                                          ,uLiteral (uInt 1)])
      `shouldSucceedWith`
      (forall [] typeInt,
       tUncurriedFnApplication (tSymbol (Unqualified "max") (typeVariadic [] typeInt typeInt))
                              [tSlice [tLiteral (tInt 0) typeInt
                                          ,tLiteral (tInt 1) typeInt] typeInt]
       typeInt)

    it "infers variadic no-arg func application" $
      inferExpr predefAndMaxVariadic (uApplication (uSymbol (Unqualified "max")) [])
      `shouldSucceedWith`
      (forall [] typeInt,
       tUncurriedFnApplication (tSymbol (Unqualified "max") (typeVariadic [] typeInt typeInt))
                              [tSlice [] typeInt]
       typeInt)

    it "infers struct initializer" $
      let structType = (TStruct Missing [TStructField Missing "msg" (TBasic Missing TString)]) in
        inferExpr predef (Untyped.StructInitializer
                          Missing
                          (TSStruct Missing [TSStructField Missing "msg" (TSSymbol Missing (Unqualified "string"))])
                          [Untyped.Literal Missing (Untyped.String "hello")])
        `shouldSucceedWith`
        (forall [] structType,
         Core.StructInitializer Missing structType [Core.Literal Missing (Core.String "hello") typeString])


  describe "inferDefinition" $ do

    it "infers (def n (+ 1 1))" $
      inferDefinition predef (uDefinition "n" Nothing (uOp
                                                              Add
                                                              (uLiteral (uInt 1))
                                                              (uLiteral (uInt 1))))
      `shouldSucceedWith`
      tDefinition
      "n"
      (forall [] typeInt,
       tOp
       Add
       (tLiteral (tInt 1) typeInt)
       (tLiteral (tInt 1) typeInt)
       typeInt)

    it "infers definition without type signature" $
      inferDefinition empty (uDefinition "x" Nothing (uLiteral (uInt 1)))
      `shouldSucceedWith`
      tDefinition "x" (forall [] typeInt, tLiteral (tInt 1) typeInt)

    it "infers polymorphic definition without type signature" $
      shouldSucceed $
        inferDefinition
          empty
          (uDefinition "id"
                              Nothing
                              (uFn (uNameBinding "x") (uSymbol (Unqualified "x"))))

    it "infers definition with type signature" $
      inferDefinition empty (uDefinition "x" (Just $ implicit (tsSymbol (Unqualified "any"))) (uLiteral (uInt 1)))
      `shouldSucceedWith`
      tDefinition "x" (forall [] typeAny, tLiteral (tInt 1) typeInt)

    it "infers polymorphic definition with type signature" $
      inferDefinition empty (uDefinition "id"
                                                (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                                                (uFn (uNameBinding "x") (uSymbol (Unqualified "x"))))
      `shouldSucceedWith`
      tDefinition "id" (forall [tvarBinding tvA] (typeFn tvarA tvarA),
                            tFn (tNameBinding "x") (tSymbol (Unqualified "x") tvarA) (typeFn tvarA tvarA))

    it "fails when specified type signature does not unify" $
      shouldFail $
        inferDefinition empty (uDefinition "some-number"
                                                  (Just $ implicit (tsSymbol (Unqualified "bool")))
                                                  (uLiteral (uInt 1)))

    it "subsumes int with any" $
        inferDefinition empty (uDefinition "some-number"
                                                  (Just $ implicit (tsSymbol (Unqualified "any")))
                                                  (uLiteral (uInt 1)))
        `shouldSucceedWith`
        tDefinition "some-number" (forall [] typeAny, tLiteral (tInt 1) typeInt)


    it "infers twice function with correct type signature" $
      inferDefinition empty (uDefinition "twice"
                                                (Just $ explicit [varBinding "a"] (tsFn (tsFn (tsVar "a") (tsVar "a")) (tsFn (tsVar "a") (tsVar "a"))))
                                                twiceUntyped)
      `shouldSucceedWith`
      twiceTyped

    it "fails on twice function with incorrect type signature" $
      shouldFail $
        inferDefinition empty (uDefinition "twice"
                                                  (Just $ explicit [varBinding "a"] (tsFn (tsVar "a") (tsVar "a")))
                                                  twiceUntyped)

    it "infers recursive definition" $
      inferDefinition predef (uDefinition "f" (Just $ implicit (tsFn (tsSymbol (Unqualified "int")) (tsSymbol (Unqualified "int")))) countToZero)
      `shouldSucceedWith`
      countToZeroTyped

    it "infers recursive definition without type signature" $
      inferDefinition predef (uDefinition "f" Nothing countToZero)
      `shouldSucceedWith`
      countToZeroTyped

    it "fails on recursive with incorrect signature" $
      shouldFail $
        inferDefinition predef (uDefinition "f" (Just $ implicit (tsFn (tsSymbol (Unqualified "int")) (tsSymbol (Unqualified "any")))) countToZero)
