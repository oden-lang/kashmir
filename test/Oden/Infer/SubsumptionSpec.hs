module Oden.Infer.SubsumptionSpec where

import           Test.Hspec

import           Oden.Infer.Subsumption
import           Oden.SourceInfo
import           Oden.Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.Predefined
import           Oden.Type.Polymorphic
import           Oden.Pretty ()

import           Oden.Assertions
import           Oden.Infer.Fixtures

spec :: Spec
spec =
  describe "subsumedBy" $ do

    it "tvar is subsumed by same tvar" $
      scheme tvarA `subsumedBy` Literal (Metadata Predefined) Unit tvarA
      `shouldSucceedWith`
      (scheme tvarA, Literal (Metadata Predefined) Unit tvarA)

    it "tvar b is subsumed by tvar a and all a tvars are substituted" $
      scheme tvarB `subsumedBy` Literal (Metadata Predefined) Unit tvarA
      `shouldSucceedWith`
      (scheme tvarB, Literal (Metadata Predefined) Unit tvarB)

    it "int is subsumed by int" $
      scheme typeInt `subsumedBy` Literal (Metadata Predefined) (Int 1) typeInt
      `shouldSucceedWith`
      (scheme typeInt, Literal (Metadata Predefined) (Int 1) typeInt)

    it "string is subsumed by string" $
      scheme typeString `subsumedBy` Literal (Metadata Predefined) (String "foo") typeString
      `shouldSucceedWith`
      (scheme typeString, Literal (Metadata Predefined) (String "foo") typeString)

    it "string is not subsumed by int" $
      shouldFail (scheme typeString `subsumedBy` Literal (Metadata Predefined) (Int 1) typeInt)

    it "int is not subsumed by string" $
      shouldFail (scheme typeInt `subsumedBy` Literal (Metadata Predefined) (String "foo") typeString)

    it "TFn of TVars is subsumed by same TFn" $
      let expr tv = Fn (Metadata Predefined)
                    (NameBinding missing (Identifier "x"))
                    (Symbol missing (Identifier "x") tv) (typeFn tv tv) in
        scheme (typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (typeFn tvarA tvarA), expr tvarA)

    it "TFn of a tvars is not subsumed by TFn from string to int" $
      let expr = Fn (Metadata Predefined)
                    (NameBinding missing (Identifier "x"))
                    (Symbol missing (Identifier "x") tvarA) (typeFn tvarA tvarA)
      in shouldFail (scheme (typeFn typeString typeInt) `subsumedBy` expr)

    it "TVar is not subsumed by TFn" $
      let expr = Symbol missing (Identifier "x") (typeFn tvarB tvarB) in
        shouldFail (scheme tvarA `subsumedBy` expr)

    it "tuple of tvars is subsumed by tuple of same tvars" $
      let tupleType = (TTuple (Metadata Predefined) tvarA tvarA [])
          expr = Symbol missing (Identifier "x") tupleType in
        scheme tupleType `subsumedBy` expr
        `shouldSucceedWith`
        (scheme tupleType, expr)

    it "tvar slice is subsumed by same tvar slice" $
      let expr tv = Slice (Metadata Predefined) [Symbol missing (Identifier "x") tv] (typeSlice tv) in
        scheme (typeSlice tvarA) `subsumedBy` expr tvarA
        `shouldSucceedWith`
        (scheme (typeSlice tvarA), expr tvarA)

    it "TNamed TFn of TVars is subsumed by unnamed but equal TFn" $
      let expr tv = Fn (Metadata Predefined)
                    (NameBinding missing (Identifier "x"))
                    (Symbol missing (Identifier "x") tv) (typeFn tv tv) in
        scheme (named "MyFn" $ typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (named "MyFn" $ typeFn tvarA tvarA), expr tvarA)

    it "empty record is subsumed by empty record" $
      let expr = Symbol missing (Identifier "x") emptyRow in
        scheme emptyRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme emptyRow, expr)

    it "empty record is subsumed by one field record" $
      let oneFieldRow = RExtension missing (Identifier "foo") typeInt emptyRow
          expr = Symbol missing (Identifier "x") oneFieldRow in
        scheme emptyRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme emptyRow, expr)

    it "one field record is subsumed by extended record" $
      let oneFieldRow = RExtension missing (Identifier "foo") typeInt emptyRow
          twoFieldRow = RExtension missing (Identifier "bar") typeString oneFieldRow
          expr = Symbol missing (Identifier "x") twoFieldRow in
        scheme oneFieldRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme oneFieldRow, expr)

    it "two field record is not subsumed by one field record" $
      let oneFieldRow = RExtension missing (Identifier "foo") typeInt emptyRow
          twoFieldRow = RExtension missing (Identifier "bar") typeString oneFieldRow
          expr = Symbol missing (Identifier "x") oneFieldRow in
        scheme twoFieldRow `subsumedBy` expr
        `shouldFailWith`
        SubsumptionError Missing twoFieldRow oneFieldRow

    it "record with row variable is subsumed by other row variable record" $
      let firstRow = RExtension missing (Identifier "foo") typeInt tvarA
          secondRow = RExtension missing (Identifier "foo") typeInt tvarB
          expr = Symbol missing (Identifier "x") secondRow in
        scheme firstRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme firstRow, expr)

    it "record with row variable is subsumed by multi-field record" $
      let firstRow = RExtension missing (Identifier "foo") typeInt tvarA
          twoFieldRow = RExtension missing (Identifier "bar") typeString (RExtension missing (Identifier "foo") typeInt (REmpty missing))
          expr = Symbol missing (Identifier "x") twoFieldRow in
        scheme firstRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme firstRow, expr)

    it "record with row variable is subsumed by multi-field record with row variable" $
      let firstRow = RExtension missing (Identifier "foo") typeInt tvarA
          twoFieldRow = RExtension missing (Identifier "bar") typeString (RExtension missing (Identifier "foo") typeInt tvarB)
          expr = Symbol missing (Identifier "x") twoFieldRow in
        scheme firstRow `subsumedBy` expr
        `shouldSucceedWith`
        (scheme firstRow, expr)

    it "{ foo: int | b } -> { foo: int | b } is subsumed by a -> a" $
      let row = RExtension missing (Identifier "bar") typeString (RExtension missing (Identifier "foo") typeInt tvarB)
          expr t = Symbol missing (Identifier "x") (TFn missing t t) in
        scheme (TFn missing row row) `subsumedBy` (expr tvarA)
        `shouldSucceedWith'`
        (scheme (TFn missing row row), expr row)

    it "{ foo: c | d } -> c is subsumed by a -> b" $
      let row = (RExtension missing (Identifier "foo") tvarC tvarD)
          expr d r = Symbol missing (Identifier "x") (TFn missing d r) in
        scheme (TFn missing row tvarC) `subsumedBy` (expr tvarA tvarB)
        `shouldSucceedWith'`
        (scheme (TFn missing row tvarC), expr row tvarC)
