-- | This module contains values representing monomorphic types.
module Oden.Type.Monomorphic where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo

data Type
  = TAny (Metadata SourceInfo)
  | TTuple (Metadata SourceInfo) Type Type [Type]
  | TCon (Metadata SourceInfo) QualifiedName
  | TNoArgFn (Metadata SourceInfo) Type
  | TFn (Metadata SourceInfo) Type Type
  | TSlice (Metadata SourceInfo) Type
  | TRecord (Metadata SourceInfo) Type
  | TNamed (Metadata SourceInfo) QualifiedName Type
  | TUncurriedFn (Metadata SourceInfo) [Type] [Type]
  | TVariadicFn (Metadata SourceInfo) [Type] Type [Type]
  -- Row
  | REmpty (Metadata SourceInfo)
  | RExtension (Metadata SourceInfo) Identifier Type Type
  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny (Metadata si))              = si
  getSourceInfo (TTuple (Metadata si) _ _ _)      = si
  getSourceInfo (TFn (Metadata si) _ _)           = si
  getSourceInfo (TNoArgFn (Metadata si) _)        = si
  getSourceInfo (TCon (Metadata si) _)            = si
  getSourceInfo (TUncurriedFn (Metadata si) _ _)  = si
  getSourceInfo (TVariadicFn (Metadata si) _ _ _) = si
  getSourceInfo (TSlice (Metadata si) _)          = si
  getSourceInfo (TRecord (Metadata si) _)         = si
  getSourceInfo (TNamed (Metadata si) _ _)        = si
  getSourceInfo (REmpty (Metadata si))            = si
  getSourceInfo (RExtension (Metadata si) _ _ _)  = si

  setSourceInfo si (TAny _)              = TAny (Metadata si)
  setSourceInfo si (TTuple _ f s r)      = TTuple (Metadata si) f s r
  setSourceInfo si (TFn _ a r)           = TFn (Metadata si) a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn (Metadata si) r
  setSourceInfo si (TCon _ n)            = TCon (Metadata si) n
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn (Metadata si) a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn (Metadata si) a v r
  setSourceInfo si (TSlice _ t)          = TSlice (Metadata si) t
  setSourceInfo si (TRecord _ fs)        = TRecord (Metadata si) fs
  setSourceInfo si (TNamed _ n t)        = TNamed (Metadata si) n t
  setSourceInfo si REmpty{}              = REmpty (Metadata si)
  setSourceInfo si (RExtension _ l t r)  = RExtension (Metadata si) l t r

rowToList :: Type -> [(Identifier, Type)]
rowToList (RExtension _ label type' row) = (label, type') : rowToList row
rowToList _ = []

rowFromList :: [(Identifier, Type)] -> Type
rowFromList ((label, type') : fields) = RExtension (Metadata Missing) label type' (rowFromList fields)
rowFromList _ = REmpty (Metadata Missing)

getLeafRow :: Type -> Type
getLeafRow (RExtension _ _ _ row) = getLeafRow row
getLeafRow e = e
