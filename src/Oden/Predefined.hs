module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Basic
import Oden.Type.Polymorphic
import Oden.SourceInfo
import qualified Oden.Core as Core

import qualified Data.Map as Map

pairs :: [(Name, Scheme)]
pairs = [
  ("len", Forall Predefined [TVarBinding Predefined (TV "a")] (TUncurriedFn Predefined [TSlice Predefined (TVar Predefined (TV "a"))] [TBasic Predefined TInt]))
  ]

predefined :: Map.Map Name Core.Definition
predefined = Map.fromList (map toAssoc pairs)
  where
  toAssoc (i, s) = (i, Core.ForeignDefinition Predefined i s)
