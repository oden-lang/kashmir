module Oden.Assertions where

import           Text.PrettyPrint.Leijen hiding ((<$>))

import           Test.Hspec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

shouldSucceed :: (Eq a, Show a, Show e) => Either e a -> Expectation
shouldSucceed  res = res `shouldSatisfy` isRight

shouldSucceedWith :: (Eq v, Show v, Show e) => Either e v -> v -> Expectation
(Left err)    `shouldSucceedWith` _         = expectationFailure . show $ err
(Right value) `shouldSucceedWith` expected  = value `shouldBe` expected

shouldFail :: (Eq a, Show a, Show e) => Either e a -> Expectation
shouldFail res = res `shouldSatisfy` isLeft

shouldFailWith :: (Eq a, Show a, Eq e, Show e) => Either e a -> e -> Expectation
res `shouldFailWith` err = res `shouldSatisfy` (== Left err)

-- PRETTY PRINTING RESULTS

newtype PrettyWrapper a = PrettyWrapper a deriving (Eq)

instance Pretty a => Show (PrettyWrapper a) where
  show (PrettyWrapper x) = displayS (renderPretty 0.4 100 (pretty x)) ""

shouldSucceed' :: (Eq a, Pretty a, Show e) => Either e a -> Expectation
shouldSucceed' = shouldSucceed . (PrettyWrapper <$>)

shouldSucceedWith' :: (Eq v, Pretty v, Show e) => Either e v -> v -> Expectation
shouldSucceedWith' res expected = (PrettyWrapper <$> res) `shouldSucceedWith` PrettyWrapper expected

shouldFail' :: (Eq a, Pretty a, Show e) => Either e a -> Expectation
shouldFail' = shouldFail . (PrettyWrapper <$>)

shouldFailWith' :: (Eq a, Pretty a, Eq e, Show e) => Either e a -> e -> Expectation
shouldFailWith' res expected = (PrettyWrapper <$> res) `shouldFailWith` expected
