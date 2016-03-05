module Oden.Compiler.InstantiateSpec where

import           Test.Hspec

import           Oden.Compiler.Instantiate
import qualified Oden.Core                 as Core
import           Oden.Identifier
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly
import           Oden.Type.Basic

import           Oden.Assertions

tvA :: Poly.TVar
tvA = Poly.TV "a"

tvarA :: Poly.Type
tvarA = Poly.TVar Missing tvA

typeInt = Poly.TBasic Missing TInt
typeBool = Poly.TBasic Missing TBool

monoInt = Mono.TBasic Missing TInt
monoBool = Mono.TBasic Missing TBool

identityPoly :: Core.Expr Poly.Type
identityPoly =
  Core.Fn
  Missing
  (Core.NameBinding Missing "x")
  (Core.Symbol Missing (Unqualified "x") tvarA)
  (Poly.TFn Missing tvarA tvarA)

identityIntType :: Mono.Type
identityIntType = Mono.TFn Missing monoInt monoInt

identityInt :: Core.Expr Poly.Type
identityInt =
  Core.Fn
  Missing
  (Core.NameBinding Missing "x")
  (Core.Symbol Missing (Unqualified "x") typeInt)
  (Poly.TFn Missing typeInt typeInt)

lenType :: Poly.Type
lenType = Poly.TUncurriedFn Missing [Poly.TSlice Missing tvarA] [typeInt]

lenPoly :: Core.Expr Poly.Type
lenPoly = Core.Symbol Missing (Unqualified "len") lenType

lenIntType :: Mono.Type
lenIntType = Mono.TUncurriedFn Missing [Mono.TSlice Missing monoInt] [monoInt]

lenInt :: Core.Expr Poly.Type
lenInt =
  Core.Symbol
  Missing
  (Unqualified "len")
  (Poly.TUncurriedFn Missing [Poly.TSlice Missing typeInt] [typeInt])

pairPoly :: Core.Expr Poly.Type
pairPoly =
  Core.Tuple
  Missing
  (Core.Symbol Missing (Unqualified "x") tvarA)
  (Core.Symbol Missing (Unqualified "y") (Poly.TVar Missing (Poly.TV "b")))
  []
  (Poly.TTuple Missing tvarA (Poly.TVar Missing (Poly.TV "b")) [])


pairIntStringType :: Mono.Type
pairIntStringType = Mono.TTuple Missing monoInt (Mono.TBasic Missing TString) []

pairIntString :: Core.Expr Poly.Type
pairIntString =
  Core.Tuple
  Missing
  (Core.Symbol Missing (Unqualified "x") typeInt)
  (Core.Symbol Missing (Unqualified "y") (Poly.TBasic Missing TString))
  []
  (Poly.TTuple Missing typeInt (Poly.TBasic Missing TString) [])

spec :: Spec
spec =
  describe "instantiate" $ do
    it "does not change monomorphic function" $
      instantiate identityInt identityIntType `shouldSucceedWith` identityInt
    it "instantiates identity function with int" $
      instantiate identityPoly identityIntType `shouldSucceedWith` identityInt
    it "instantiates len func" $
      instantiate lenPoly lenIntType `shouldSucceedWith` lenInt
    it "instantiates polymorphic tuple" $
      instantiate pairPoly pairIntStringType `shouldSucceedWith` pairIntString
