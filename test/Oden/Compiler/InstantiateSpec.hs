module Oden.Compiler.InstantiateSpec where

import           Test.Hspec

import           Oden.Compiler.Instantiate
import qualified Oden.Core                 as Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

tvA :: Poly.TVar
tvA = Poly.TV "a"

tvB :: Poly.TVar
tvB = Poly.TV "b"

tvarA :: Poly.Type
tvarA = Poly.TVar missing tvA

tvarB :: Poly.Type
tvarB = Poly.TVar missing tvB

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")
typeString = Poly.TCon missing (nameInUniverse "string")

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")
monoString = Mono.TCon missing (nameInUniverse "string")

identityPoly :: Core.Expr Poly.Type
identityPoly =
  Core.Fn
  missing
  (Core.NameBinding missing (Identifier "x"))
  (Core.Symbol missing (Identifier "x") tvarA)
  (Poly.TFn missing tvarA tvarA)

identityIntType :: Mono.Type
identityIntType = Mono.TFn missing monoInt monoInt

identityInt :: Core.Expr Poly.Type
identityInt =
  Core.Fn
  missing
  (Core.NameBinding missing (Identifier "x"))
  (Core.Symbol missing (Identifier "x") typeInt)
  (Poly.TFn missing typeInt typeInt)

lenType :: Poly.Type
lenType = Poly.TUncurriedFn missing [Poly.TSlice missing tvarA] [typeInt]

lenPoly :: Core.Expr Poly.Type
lenPoly = Core.Symbol missing (Identifier "len") lenType

lenIntType :: Mono.Type
lenIntType = Mono.TUncurriedFn missing [Mono.TSlice missing monoInt] [monoInt]

lenInt :: Core.Expr Poly.Type
lenInt =
  Core.Symbol
  missing
  (Identifier "len")
  (Poly.TUncurriedFn missing [Poly.TSlice missing typeInt] [typeInt])

pairPoly :: Core.Expr Poly.Type
pairPoly =
  Core.Tuple
  missing
  (Core.Symbol missing (Identifier "x") tvarA)
  (Core.Symbol missing (Identifier "y") (Poly.TVar missing (Poly.TV "b")))
  []
  (Poly.TTuple missing tvarA (Poly.TVar missing (Poly.TV "b")) [])


pairIntStringType :: Mono.Type
pairIntStringType = Mono.TTuple missing monoInt monoString []

pairIntString :: Core.Expr Poly.Type
pairIntString =
  Core.Tuple
  missing
  (Core.Symbol missing (Identifier "x") typeInt)
  (Core.Symbol missing (Identifier "y") typeString)
  []
  (Poly.TTuple missing typeInt typeString [])

recordPoly :: Core.Expr Poly.Type
recordPoly =
  Core.RecordInitializer
  missing
  (Poly.TRecord
   missing
   (Poly.RExtension missing (Identifier "foo") tvarA (Poly.REmpty missing)))
  [Core.FieldInitializer missing (Identifier "foo") (Core.Symbol missing (Identifier "x") tvarA)]

recordIntType :: Mono.Type
recordIntType =
  (Mono.TRecord
   missing
   (Mono.RExtension missing (Identifier "foo") monoInt (Mono.REmpty missing)))

recordInt :: Core.Expr Poly.Type
recordInt =
  Core.RecordInitializer
  missing
  (Poly.TRecord
   missing
   (Poly.RExtension missing (Identifier "foo") typeInt (Poly.REmpty missing)))
  [Core.FieldInitializer missing (Identifier "foo") (Core.Symbol missing (Identifier "x") typeInt)]

polyRecordIntAndRowVariable :: Poly.Type
polyRecordIntAndRowVariable =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   tvarA)

polyRecordInt :: Poly.Type
polyRecordInt =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.REmpty missing))

polyRecordIntAndString :: Poly.Type
polyRecordIntAndString =
  Poly.TRecord
  missing
  (Poly.RExtension
   missing
   (Identifier "a")
   typeInt
   (Poly.RExtension
    missing
    (Identifier "b")
    typeString
    (Poly.REmpty missing)))

fieldAccessPoly :: Poly.Type -> Core.Expr Poly.Type
fieldAccessPoly recordType =
  Core.Fn
  missing
  (Core.NameBinding missing (Identifier "x"))
  (Core.RecordFieldAccess
   missing
   (Core.Symbol missing (Identifier "x") recordType)
   (Identifier "a")
   typeInt)
  (Poly.TFn
   missing
   recordType
   typeInt)

fieldAccessOnlyIntType :: Mono.Type
fieldAccessOnlyIntType =
  Mono.TFn
  missing
  (Mono.TRecord missing (Mono.RExtension missing (Identifier "a") monoInt (Mono.REmpty missing)))
  monoInt

monoRecordEmpty :: Mono.Type
monoRecordEmpty = Mono.TRecord missing (Mono.REmpty missing)

monoRecordInt :: Mono.Type
monoRecordInt =
  Mono.TRecord
  missing
  (Mono.RExtension
   missing
   (Identifier "a")
   monoInt
   (Mono.REmpty missing))

monoRecordIntAndString :: Mono.Type
monoRecordIntAndString =
  Mono.TRecord
  missing
  (Mono.RExtension
   missing
   (Identifier "a")
   monoInt
   (Mono.RExtension
    missing
    (Identifier "b")
    monoString
    (Mono.REmpty missing)))

monoFieldAccess r = Mono.TFn missing r monoInt

fieldAccessWrongLabelType :: Mono.Type
fieldAccessWrongLabelType =
  Mono.TFn
  missing
  (Mono.TRecord missing (Mono.RExtension missing (Identifier "b") monoInt (Mono.REmpty missing)))
  monoInt

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
    it "instantiates record initializer with int field" $
      instantiate recordPoly recordIntType `shouldSucceedWith` recordInt
    it "instantiates record field access with only an int field" $
      instantiate (fieldAccessPoly polyRecordIntAndRowVariable) (monoFieldAccess monoRecordInt) `shouldSucceedWith` (fieldAccessPoly polyRecordInt)
    it "instantiates record field access with both int and string field" $
      instantiate (fieldAccessPoly polyRecordIntAndRowVariable) (monoFieldAccess monoRecordIntAndString) `shouldSucceedWith` (fieldAccessPoly polyRecordIntAndString)
    it "fails on mismatching row labels" $
      shouldFail $
        instantiate (fieldAccessPoly polyRecordIntAndRowVariable) fieldAccessWrongLabelType
