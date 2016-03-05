module Oden.Compiler.MonomorphizationSpec where

import           Data.Set              as Set
import           Test.Hspec

import           Oden.Compiler.Environment
import           Oden.Compiler.Monomorphization
import qualified Oden.Core             as Core
import           Oden.Identifier
import           Oden.Predefined
import qualified Oden.Environment as Environment
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly
import           Oden.Type.Basic

import           Oden.Assertions

myPkg :: Core.PackageDeclaration
myPkg = Core.PackageDeclaration Missing ["my", "pkg"]

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar Missing tvA

typeInt = Poly.TBasic Missing TInt
typeBool = Poly.TBasic Missing TBool

monoInt = Mono.TBasic Missing TInt
monoBool = Mono.TBasic Missing TBool

identityDef :: Core.Definition
identityDef =
  Core.Definition
    Missing
    "identity"
    (Poly.Forall Missing [Poly.TVarBinding Missing tvA] (Poly.TFn Missing a a),
     Core.Fn Missing (Core.NameBinding Missing "x") (Core.Symbol Missing (Unqualified "x") a)
                 (Poly.TFn Missing a a))

identity2Def :: Core.Definition
identity2Def =
  Core.Definition
    Missing
    "identity2"
    (Poly.Forall Missing [Poly.TVarBinding Missing tvA] (Poly.TFn Missing a a),
     Core.Fn Missing (Core.NameBinding Missing "x") (Core.Application Missing (Core.Symbol Missing (Unqualified "identity") (Poly.TFn Missing a a))
                                   (Core.Symbol Missing (Unqualified "x") a)
                                   a)
                 (Poly.TFn Missing a a))

usingIdentityDef :: Core.Definition
usingIdentityDef =
  Core.Definition
    Missing
    "using-identity"
    (Poly.Forall Missing [] typeInt,
     Core.Application Missing
                      (Core.Symbol Missing (Unqualified "identity") (Poly.TFn Missing typeInt typeInt))
                      (Core.Literal Missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentity2Def :: Core.Definition
usingIdentity2Def =
  Core.Definition
    Missing
    "using-identity2"
    (Poly.Forall Missing [] typeInt,
     Core.Application Missing
                      (Core.Symbol Missing (Unqualified "identity2") (Poly.TFn Missing typeInt typeInt))
                      (Core.Literal Missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentityMonomorphed :: MonomorphedDefinition
usingIdentityMonomorphed =
  MonomorphedDefinition
    Missing
    "using-identity"
    monoInt
    (Core.Application Missing
                      (Core.Symbol Missing (Unqualified "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
                      (Core.Literal Missing (Core.Int 1) monoInt)
                      monoInt)

letBoundIdentity :: Core.Definition
letBoundIdentity =
  Core.Definition
    Missing
    "let-bound-identity"
    (Poly.Forall Missing [] typeInt,
     Core.Let Missing (Core.NameBinding Missing "identity") (Core.Fn Missing (Core.NameBinding Missing "x") (Core.Symbol Missing (Unqualified "x") a) (Poly.TFn Missing a a))
                         (Core.Application
                          Missing
                          (Core.Symbol Missing (Unqualified "identity") (Poly.TFn Missing typeInt typeInt))
                          (Core.Literal Missing (Core.Int 1) typeInt)
                          typeInt)
                      typeInt)

usingIdentity2Monomorphed :: MonomorphedDefinition
usingIdentity2Monomorphed =
  MonomorphedDefinition
    Missing
    "using-identity2"
    monoInt
    (Core.Application
     Missing
     (Core.Symbol Missing (Unqualified "identity2_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
     (Core.Literal Missing (Core.Int 1) monoInt)
     monoInt)

letBoundIdentityMonomorphed :: MonomorphedDefinition
letBoundIdentityMonomorphed =
  MonomorphedDefinition
    Missing
    "let-bound-identity"
    monoInt
    (Core.Let
     Missing
     (Core.NameBinding Missing "identity_inst_int_to_int")
     (Core.Fn Missing (Core.NameBinding Missing "x") (Core.Symbol Missing (Unqualified "x") monoInt) (Mono.TFn Missing monoInt monoInt))
     (Core.Application Missing (Core.Symbol Missing (Unqualified "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
      (Core.Literal Missing (Core.Int 1) monoInt)
      monoInt)
     monoInt)

identityInstIntToInt :: InstantiatedDefinition
identityInstIntToInt =
  InstantiatedDefinition
    "identity_inst_int_to_int"
    (Core.Fn
     Missing
     (Core.NameBinding Missing "x")
     (Core.Symbol Missing (Unqualified "x") monoInt)
     (Mono.TFn Missing monoInt monoInt))

identity2InstIntToInt :: InstantiatedDefinition
identity2InstIntToInt =
  InstantiatedDefinition
    "identity2_inst_int_to_int"
    (Core.Fn
     Missing
     (Core.NameBinding Missing "x")
     (Core.Application
      Missing
      (Core.Symbol Missing (Unqualified "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
      (Core.Symbol Missing (Unqualified "x") monoInt)
      monoInt)
     (Mono.TFn Missing monoInt monoInt))

sliceLenDef :: Core.Definition
sliceLenDef =
  Core.Definition
    Missing
    "slice-len"
    (Poly.Forall Missing [] typeInt,
     Core.UncurriedFnApplication
      Missing
      (Core.Symbol Missing (Unqualified "len") (Poly.TUncurriedFn Missing [Poly.TSlice Missing typeBool] [typeInt]))
      [Core.Slice Missing [Core.Literal Missing (Core.Bool True) typeBool] (Poly.TSlice Missing typeBool)]
      typeInt)

sliceLenMonomorphed :: MonomorphedDefinition
sliceLenMonomorphed =
  MonomorphedDefinition
    Missing
    "slice-len"
    monoInt
    (Core.UncurriedFnApplication
     Missing
     (Core.Symbol Missing (Unqualified "len") (Mono.TUncurriedFn Missing [Mono.TSlice Missing monoBool] monoInt))
     [Core.Slice Missing [Core.Literal Missing (Core.Bool True) monoBool] (Mono.TSlice Missing (Mono.TBasic Missing TBool))]
     monoInt)

letWithShadowing :: Core.Definition
letWithShadowing =
  Core.Definition
    Missing
    "let-with-shadowing"
    (Poly.Forall Missing [] typeInt,
     Core.Let
     Missing
     (Core.NameBinding Missing "x")
     (Core.Literal Missing (Core.Int 1) typeInt)
     (Core.Let
      Missing
      (Core.NameBinding Missing "x")
      (Core.Symbol Missing (Unqualified "x") typeInt)
      (Core.Symbol Missing (Unqualified "x") typeInt)
      typeInt)
     typeInt)

letWithShadowingMonomorphed :: MonomorphedDefinition
letWithShadowingMonomorphed =
  MonomorphedDefinition
    Missing
    "let-with-shadowing"
    monoInt
    (Core.Let
     Missing
     (Core.NameBinding Missing "x")
     (Core.Literal Missing (Core.Int 1) monoInt)
     (Core.Let
      Missing
      (Core.NameBinding Missing "x")
      (Core.Symbol Missing (Unqualified "x") monoInt)
      (Core.Symbol Missing (Unqualified "x") monoInt)
      monoInt)
     monoInt)

spec :: Spec
spec =
  describe "monomorphPackage" $ do
    it "compiles empty package" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "excludes unused polymorphic definitions" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [identityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "monomorphs polymorphic function usage" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [identityDef, usingIdentityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.singleton identityInstIntToInt)
        (Set.singleton usingIdentityMonomorphed)
    it "monomorphs polymorphic function usage recursively" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [identityDef, identity2Def, usingIdentity2Def])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.fromList [identityInstIntToInt, identity2InstIntToInt])
        (Set.singleton usingIdentity2Monomorphed)
    it "monomorphs let bound polymorphic function" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [letBoundIdentity])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letBoundIdentityMonomorphed)
    it "uses polymorphic predefined Go funcs" $
      monomorphPackage (fromDefinitions predefined) (Core.Package myPkg [] [sliceLenDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton sliceLenMonomorphed)
    it "monomorphs let with shadowing" $
      monomorphPackage Environment.empty (Core.Package myPkg [] [letWithShadowing])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letWithShadowingMonomorphed)
