module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Monomorphic

missing :: Metadata SourceInfo
missing = Metadata Missing

con = TCon missing . FQN [] . Identifier

spec :: Spec
spec =
  describe "encodeTypeInstance" $ do
    it "encodes arrow" $
      encodeTypeInstance (Identifier "foo") (TFn missing (con "int") (con "string")) `shouldBe` "foo_inst_int_to_string"
    it "encodes nested arrows" $
      encodeTypeInstance (Identifier "foo") (TFn missing (con "bool") (TFn missing (con "int") (con "string"))) `shouldBe` "foo_inst_bool_to_int__to__string"
    it "encodes single arrow" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (con "int")) `shouldBe` "foo_inst_to_int"
    it "encodes nested single arrows" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (TNoArgFn missing (con "int"))) `shouldBe` "foo_inst_to_to__int"
    it "encodes uncurried func" $
      encodeTypeInstance (Identifier "foo") (TUncurriedFn missing [con "bool", con "int"] [con "string"]) `shouldBe` "foo_inst_bool_to_int_to_string"
    it "encodes uncurried funcs with multiple return values" $
      encodeTypeInstance (Identifier "foo") (TUncurriedFn missing [con "bool", con "int"] [con "string", con "int"]) `shouldBe` "foo_inst_bool_to_int_to_tupleof__string__int__"
    it "encodes variadic func" $
      encodeTypeInstance (Identifier "foo") (TVariadicFn missing [con "bool"] (con "int") [con "string"]) `shouldBe` "foo_inst_bool_to_variadic_int_to_string"
    it "encodes variadic func with multiple return values" $
      encodeTypeInstance (Identifier "foo") (TVariadicFn missing [con "bool"] (con "int") [con "string", con "int"]) `shouldBe` "foo_inst_bool_to_variadic_int_to_tupleof__string__int__"
    it "encodes slice" $
      encodeTypeInstance (Identifier "foo") (TFn missing (TSlice missing (con "bool")) (TSlice missing (con "int"))) `shouldBe` "foo_inst_sliceof__bool_to_sliceof__int"
