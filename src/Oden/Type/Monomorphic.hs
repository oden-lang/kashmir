-- | This module contains values representing monomorphic types.
module Oden.Type.Monomorphic where

import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Basic

data StructField = TStructField SourceInfo String Type
                 deriving (Show, Eq, Ord)

data Type
  = TAny SourceInfo
  | TBasic SourceInfo BasicType
  | TUnit SourceInfo
  | TTuple SourceInfo Type Type [Type]
  | TCon SourceInfo Type Type
  | TNoArgFn SourceInfo Type
  | TFn SourceInfo Type Type
  | TUncurriedFn SourceInfo [Type] [Type]
  | TVariadicFn SourceInfo [Type] Type Type
  | TSlice SourceInfo Type
  | TStruct SourceInfo [StructField]
  | TNamed SourceInfo QualifiedName Type
  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny si)              = si
  getSourceInfo (TBasic si _)          = si
  getSourceInfo (TUnit si)             = si
  getSourceInfo (TTuple si _ _ _)      = si
  getSourceInfo (TFn si _ _)           = si
  getSourceInfo (TNoArgFn si _)        = si
  getSourceInfo (TCon si _ _)          = si
  getSourceInfo (TUncurriedFn si _ _)  = si
  getSourceInfo (TVariadicFn si _ _ _) = si
  getSourceInfo (TSlice si _)          = si
  getSourceInfo (TStruct si _)         = si
  getSourceInfo (TNamed si _ _)        = si

  setSourceInfo si (TAny _)              = TAny si
  setSourceInfo si (TBasic _ b)          = TBasic si b
  setSourceInfo si (TUnit _)             = TUnit si
  setSourceInfo si (TTuple _ f s r)      = TTuple si f s r
  setSourceInfo si (TFn _ a r)           = TFn si a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn si r
  setSourceInfo si (TCon _ d r)          = TCon si d r
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn si a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn si a v r
  setSourceInfo si (TSlice _ t)          = TSlice si t
  setSourceInfo si (TStruct _ fs)        = TStruct si fs
  setSourceInfo si (TNamed _ n t)        = TNamed si n t
