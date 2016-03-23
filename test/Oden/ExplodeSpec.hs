module Oden.ExplodeSpec where

import qualified Oden.Core.Untyped     as U
import qualified Oden.Explode          as E
import           Oden.Explode          hiding (explodeTopLevel)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Type.Signature
import           Test.Hspec

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<test>" l c)

ignored :: Metadata SourceInfo
ignored = Metadata Missing

explodeTopLevel = E.explodeTopLevel ["pkg"]

spec :: Spec
spec = do
  describe "explodeExpr" $ do
    it "converts symbol" $
       explodeExpr (Symbol (src 1 1) (Identifier "x"))
      `shouldSucceedWith`
      U.Symbol ignored (Identifier "x")

    it "converts int literal" $
       explodeExpr (Literal (src 1 1) (Int 1))
      `shouldSucceedWith`
      U.Literal ignored (U.Int 1)

    it "converts bool literal" $
       explodeExpr (Literal (src 1 1) (Bool True))
      `shouldSucceedWith`
      U.Literal ignored (U.Bool True)

    it "converts fn application with no params" $
       explodeExpr (Application
                   (src 1 1)
                   (Fn (src 1 1) [] (Symbol (src 1 3) (Identifier "x")))
                   [])
      `shouldSucceedWith`
      U.Application
      ignored
      (U.NoArgFn ignored (U.Symbol ignored (Identifier "x"))) []

    it "converts fn application with multiple params" $
       explodeExpr (Application
                   (src 1 1)
                   (Fn
                    (src 1 1)
                    [NameBinding (src 1 2) (Identifier "x"),
                     NameBinding (src 1 3) (Identifier "y")]
                    (Symbol (src 1 4) (Identifier "x")))
                   [Symbol (src 1 5) (Identifier "x"),
                    Symbol (src 1 9) (Identifier "y")])
      `shouldSucceedWith`
      U.Application
      ignored
      (U.Fn
       ignored
       (U.NameBinding ignored (Identifier "x"))
       (U.Fn
        ignored
        (U.NameBinding ignored (Identifier "y"))
        (U.Symbol ignored (Identifier "x"))))
      [U.Symbol ignored (Identifier "x"),
       U.Symbol ignored (Identifier "y")]

  describe "explodeTopLevel" $ do
    it "converts fn definition with no argument" $
       snd <$> explodeTopLevel [FnDefinition (src 1 1) (Identifier "f") [] (Symbol (src 1 3) (Identifier "x"))]
      `shouldSucceedWith`
      [U.Definition ignored (Identifier "f") Nothing (U.NoArgFn ignored (U.Symbol ignored (Identifier "x")))]

    it "converts fn definition with single argument" $
       snd <$> explodeTopLevel [FnDefinition
                                (src 1 1)
                                (Identifier "f")
                                [NameBinding (src 1 2) (Identifier "x")]
                                (Symbol (src 1 3) (Identifier "x"))]
      `shouldSucceedWith`
      [U.Definition
       ignored
       (Identifier "f")
       Nothing
       (U.Fn
        ignored
        (U.NameBinding ignored (Identifier "x"))
        (U.Symbol ignored (Identifier "x")))]

    it "converts fn definition with multiple arguments" $
       snd <$> explodeTopLevel [FnDefinition
                                (src 1 1)
                                (Identifier "f")
                                [NameBinding (src 1 2) (Identifier "x"), NameBinding (src 1 3) (Identifier "y")]
                                (Symbol (src 1 4) (Identifier "x"))]
      `shouldSucceedWith`
      [U.Definition
       ignored
       (Identifier "f")
       Nothing
       (U.Fn
        ignored
        (U.NameBinding ignored (Identifier "x"))
        (U.Fn
         ignored
         (U.NameBinding ignored (Identifier "y"))
         (U.Symbol ignored (Identifier "x"))))]

    it "converts struct definition and uses empty list for type parameters" $
       snd <$> explodeTopLevel [TypeDefinition
                                (src 1 1)
                                (Identifier "S")
                                (TSStruct (src 1 2) [TSStructField (src 1 3) (Identifier "x") (TSSymbol (src 1 4) (Identifier "t"))])]
      `shouldSucceedWith`
      [U.TypeDefinition
       ignored
       (FQN ["pkg"] (Identifier "S"))
       []
       (TSStruct (src 1 2) [TSStructField (src 1 3) (Identifier "x") (TSSymbol (src 1 4) (Identifier "t"))])]
