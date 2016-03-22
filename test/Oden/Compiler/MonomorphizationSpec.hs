module Oden.Compiler.MonomorphizationSpec where

import           Data.Set              as Set
import           Test.Hspec

import           Oden.Compiler.Monomorphization
import qualified Oden.Core             as Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

myPkg :: Core.PackageDeclaration
myPkg = Core.PackageDeclaration missing ["my", "pkg"]

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar missing tvA

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")

identityDef :: Core.Definition
identityDef =
  Core.Definition
    missing
    (Identifier "identity")
    (Poly.Forall missing [Poly.TVarBinding missing tvA] (Poly.TFn missing a a),
     Core.Fn missing (Core.NameBinding missing (Identifier "x")) (Core.Symbol missing (Identifier "x") a)
                 (Poly.TFn missing a a))

identity2Def :: Core.Definition
identity2Def =
  Core.Definition
    missing
    (Identifier "identity2")
    (Poly.Forall missing [Poly.TVarBinding missing tvA] (Poly.TFn missing a a),
     Core.Fn missing (Core.NameBinding missing (Identifier "x")) (Core.Application missing (Core.Symbol missing (Identifier "identity") (Poly.TFn missing a a))
                                   (Core.Symbol missing (Identifier "x") a)
                                   a)
                 (Poly.TFn missing a a))

usingIdentityDef :: Core.Definition
usingIdentityDef =
  Core.Definition
    missing
    (Identifier "using-identity")
    (Poly.Forall missing [] typeInt,
     Core.Application missing
                      (Core.Symbol missing (Identifier "identity") (Poly.TFn missing typeInt typeInt))
                      (Core.Literal missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentity2Def :: Core.Definition
usingIdentity2Def =
  Core.Definition
    missing
    (Identifier "using-identity2")
    (Poly.Forall missing [] typeInt,
     Core.Application missing
                      (Core.Symbol missing (Identifier "identity2") (Poly.TFn missing typeInt typeInt))
                      (Core.Literal missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentityMonomorphed :: MonomorphedDefinition
usingIdentityMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "using-identity")
    monoInt
    (Core.Application missing
                      (Core.Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
                      (Core.Literal missing (Core.Int 1) monoInt)
                      monoInt)

letBoundIdentity :: Core.Definition
letBoundIdentity =
  Core.Definition
    missing
    (Identifier "let-bound-identity")
    (Poly.Forall missing [] typeInt,
     Core.Let missing (Core.NameBinding missing (Identifier "identity")) (Core.Fn missing (Core.NameBinding missing (Identifier "x")) (Core.Symbol missing (Identifier "x") a) (Poly.TFn missing a a))
                         (Core.Application
                          missing
                          (Core.Symbol missing (Identifier "identity") (Poly.TFn missing typeInt typeInt))
                          (Core.Literal missing (Core.Int 1) typeInt)
                          typeInt)
                      typeInt)

usingIdentity2Monomorphed :: MonomorphedDefinition
usingIdentity2Monomorphed =
  MonomorphedDefinition
    missing
    (Identifier "using-identity2")
    monoInt
    (Core.Application
     missing
     (Core.Symbol missing (Identifier "identity2_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
     (Core.Literal missing (Core.Int 1) monoInt)
     monoInt)

letBoundIdentityMonomorphed :: MonomorphedDefinition
letBoundIdentityMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "let-bound-identity")
    monoInt
    (Core.Let
     missing
     (Core.NameBinding missing (Identifier "identity_inst_int_to_int"))
     (Core.Fn missing (Core.NameBinding missing (Identifier "x")) (Core.Symbol missing (Identifier "x") monoInt) (Mono.TFn missing monoInt monoInt))
     (Core.Application missing (Core.Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
      (Core.Literal missing (Core.Int 1) monoInt)
      monoInt)
     monoInt)

identityInstIntToInt :: InstantiatedDefinition
identityInstIntToInt =
  InstantiatedDefinition
    (Identifier "identity")
    missing
    (Identifier "identity_inst_int_to_int")
    (Core.Fn
     missing
     (Core.NameBinding missing (Identifier "x"))
     (Core.Symbol missing (Identifier "x") monoInt)
     (Mono.TFn missing monoInt monoInt))

identity2InstIntToInt :: InstantiatedDefinition
identity2InstIntToInt =
  InstantiatedDefinition
    (Identifier "identity2")
    missing
    (Identifier "identity2_inst_int_to_int")
    (Core.Fn
     missing
     (Core.NameBinding missing (Identifier "x"))
     (Core.Application
      missing
      (Core.Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
      (Core.Symbol missing (Identifier "x") monoInt)
      monoInt)
     (Mono.TFn missing monoInt monoInt))

sliceLenDef :: Core.Definition
sliceLenDef =
  Core.Definition
    missing
    (Identifier "slice-len")
    (Poly.Forall missing [] typeInt,
     Core.UncurriedFnApplication
      missing
      (Core.Symbol missing (Identifier "len") (Poly.TUncurriedFn missing [Poly.TSlice missing typeBool] [typeInt]))
      [Core.Slice missing [Core.Literal missing (Core.Bool True) typeBool] (Poly.TSlice missing typeBool)]
      typeInt)

sliceLenMonomorphed :: MonomorphedDefinition
sliceLenMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "slice-len")
    monoInt
    (Core.UncurriedFnApplication
     missing
     (Core.Symbol missing (Identifier "len") (Mono.TUncurriedFn missing [Mono.TSlice missing monoBool] [monoInt]))
     [Core.Slice missing [Core.Literal missing (Core.Bool True) monoBool] (Mono.TSlice missing monoBool)]
     monoInt)

letWithShadowing :: Core.Definition
letWithShadowing =
  Core.Definition
    missing
    (Identifier "let-with-shadowing")
    (Poly.Forall missing [] typeInt,
     Core.Let
     missing
     (Core.NameBinding missing (Identifier "x"))
     (Core.Literal missing (Core.Int 1) typeInt)
     (Core.Let
      missing
      (Core.NameBinding missing (Identifier "x"))
      (Core.Symbol missing (Identifier "x") typeInt)
      (Core.Symbol missing (Identifier "x") typeInt)
      typeInt)
     typeInt)

letWithShadowingMonomorphed :: MonomorphedDefinition
letWithShadowingMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "let-with-shadowing")
    monoInt
    (Core.Let
     missing
     (Core.NameBinding missing (Identifier "x"))
     (Core.Literal missing (Core.Int 1) monoInt)
     (Core.Let
      missing
      (Core.NameBinding missing (Identifier "x"))
      (Core.Symbol missing (Identifier "x") monoInt)
      (Core.Symbol missing (Identifier "x") monoInt)
      monoInt)
     monoInt)

spec :: Spec
spec =
  describe "monomorphPackage" $ do
    it "compiles empty package" $
      monomorphPackage (Core.Package myPkg [] [])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "excludes unused polymorphic definitions" $
      monomorphPackage (Core.Package myPkg [] [identityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "monomorphs polymorphic function usage" $
      monomorphPackage (Core.Package myPkg [] [identityDef, usingIdentityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.singleton identityInstIntToInt)
        (Set.singleton usingIdentityMonomorphed)
    it "monomorphs polymorphic function usage recursively" $
      monomorphPackage (Core.Package myPkg [] [identityDef, identity2Def, usingIdentity2Def])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.fromList [identityInstIntToInt, identity2InstIntToInt])
        (Set.singleton usingIdentity2Monomorphed)
    it "monomorphs let bound polymorphic function" $
      monomorphPackage (Core.Package myPkg [] [letBoundIdentity])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letBoundIdentityMonomorphed)
    it "uses polymorphic predefined Go funcs" $
      monomorphPackage (Core.Package myPkg [] [sliceLenDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton sliceLenMonomorphed)
    it "monomorphs let with shadowing" $
      monomorphPackage (Core.Package myPkg [] [letWithShadowing])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letWithShadowingMonomorphed)
