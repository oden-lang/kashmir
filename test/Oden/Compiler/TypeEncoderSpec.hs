module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Monomorphic
import           Oden.Type.Basic

spec :: Spec
spec =
  describe "encodeTypeInstance" $ do
    it "encodes arrow" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TBasic Missing TInt) (TBasic Missing TString)) `shouldBe` "foo_inst_int_to_string"
    it "encodes nested arrows" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TBasic Missing TBool) (TFn Missing (TBasic Missing TInt) (TBasic Missing TString))) `shouldBe` "foo_inst_bool_to_int__to__string"
    it "encodes single arrow" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn Missing (TBasic Missing TInt)) `shouldBe` "foo_inst_to_int"
    it "encodes nested single arrows" $
      encodeTypeInstance (Unqualified "foo") (TNoArgFn Missing (TNoArgFn Missing (TBasic Missing TInt))) `shouldBe` "foo_inst_to_to__int"
    it "encodes uncurried func" $
      encodeTypeInstance (Unqualified "foo") (TUncurriedFn Missing [TBasic Missing TBool, TBasic Missing TInt] [TBasic Missing TString]) `shouldBe` "foo_inst_bool_to_int_to_string"
    it "encodes uncurried funcs with multipe return values" $
      encodeTypeInstance (Unqualified "foo") (TUncurriedFn Missing [TBasic Missing TBool, TBasic Missing TInt] [TBasic Missing TString, TBasic Missing TInt]) `shouldBe` "foo_inst_bool_to_int_to_tupleof__string__int__"
    it "encodes variadic func" $
      encodeTypeInstance (Unqualified "foo") (TVariadicFn Missing [TBasic Missing TBool] (TBasic Missing TInt) (TBasic Missing TString)) `shouldBe` "foo_inst_bool_to_variadic_int_to_string"
    it "encodes slice" $
      encodeTypeInstance (Unqualified "foo") (TFn Missing (TSlice Missing (TBasic Missing TBool)) (TSlice Missing (TBasic Missing TInt))) `shouldBe` "foo_inst_sliceof__bool_to_sliceof__int"
