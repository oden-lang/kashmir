{-# LANGUAGE TupleSections #-}
-- | This module contains values representing polymorphic types, i.e. types
-- that can be instantiated into other polymorphic types and purely monomorphic
-- types.
module Oden.Type.Polymorphic (
  TVar(..),
  Type(..),
  TVarBinding(..),
  Scheme(..),
  rowToList,
  rowFromList,
  getLeafRow,
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  FTV,
  ftv,
  getBindingVar,
  underlying
) where

import           Oden.Identifier
import           Oden.Metadata
import qualified Oden.Type.Monomorphic as Mono
import           Oden.SourceInfo
import           Oden.QualifiedName

import qualified Data.Set               as Set

-- | Wrapper for type variable names.
newtype TVar = TV String
  deriving (Show, Eq, Ord)

-- | A polymorphic type.
data Type
  = TAny (Metadata SourceInfo)
  -- | A type variable.
  | TVar (Metadata SourceInfo) TVar
  -- | A tuple, with at least two elements.
  | TTuple (Metadata SourceInfo) Type Type [Type]
  -- | A type constructor.
  | TCon (Metadata SourceInfo) QualifiedName
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn (Metadata SourceInfo) Type
  -- | A curried function.
  | TFn (Metadata SourceInfo) Type Type
  -- | A slice type.
  | TSlice (Metadata SourceInfo) Type
  -- | A data structure, parameterized by a row for its fields.
  | TRecord (Metadata SourceInfo) Type
  -- | A name for a type, introduced by type definitions.
  | TNamed (Metadata SourceInfo) QualifiedName Type

  -- For foreign definitions:

  -- | A function that can have multiple arguments (no currying).
  | TUncurriedFn (Metadata SourceInfo) [Type] [Type]
  -- | A variadic function.
  | TVariadicFn (Metadata SourceInfo) [Type] Type [Type]

  -- | The empty row.
  | REmpty (Metadata SourceInfo)
  -- | A row extension (label and type extending another row).
  | RExtension (Metadata SourceInfo) Identifier Type Type

  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny (Metadata si))              = si
  getSourceInfo (TVar (Metadata si) _)            = si
  getSourceInfo (TTuple (Metadata si) _ _ _)      = si
  getSourceInfo (TFn (Metadata si) _ _)           = si
  getSourceInfo (TNoArgFn (Metadata si) _)        = si
  getSourceInfo (TCon (Metadata si) _)            = si
  getSourceInfo (TSlice (Metadata si) _)          = si
  getSourceInfo (TRecord (Metadata si) _)         = si
  getSourceInfo (TNamed (Metadata si) _ _)        = si
  getSourceInfo (TUncurriedFn (Metadata si) _ _)  = si
  getSourceInfo (TVariadicFn (Metadata si) _ _ _) = si
  getSourceInfo (REmpty (Metadata si))            = si
  getSourceInfo (RExtension (Metadata si) _ _ _)  = si

  setSourceInfo si (TAny _)              = TAny (Metadata si)
  setSourceInfo si (TVar _ v)            = TVar (Metadata si) v
  setSourceInfo si (TTuple _ f s r)      = TTuple (Metadata si) f s r
  setSourceInfo si (TFn _ a r)           = TFn (Metadata si) a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn (Metadata si) r
  setSourceInfo si (TCon _ n)            = TCon (Metadata si) n
  setSourceInfo si (TSlice _ t)          = TSlice (Metadata si) t
  setSourceInfo si (TRecord _ fs)        = TRecord (Metadata si) fs
  setSourceInfo si (TNamed _ n t)        = TNamed (Metadata si) n t
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn (Metadata si) a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn (Metadata si) a v r
  setSourceInfo si REmpty{}              = REmpty (Metadata si)
  setSourceInfo si (RExtension _ l t r)  = RExtension (Metadata si) l t r

-- | A type variable binding.
data TVarBinding = TVarBinding (Metadata SourceInfo) TVar
                 deriving (Show, Eq, Ord)

instance FTV TVarBinding where
  ftv (TVarBinding _ tv) = Set.singleton tv

getBindingVar :: TVarBinding -> TVar
getBindingVar (TVarBinding _ v)  = v

instance HasSourceInfo TVarBinding where
  getSourceInfo (TVarBinding (Metadata si) _)   = si
  setSourceInfo si (TVarBinding _ v) = TVarBinding (Metadata si) v

-- | A polymorphic type and its quantified type variable bindings.
data Scheme = Forall (Metadata SourceInfo) [TVarBinding] Type
            deriving (Show, Eq, Ord)

instance HasSourceInfo Scheme where
  getSourceInfo (Forall (Metadata si) _ _) = si
  setSourceInfo si (Forall _ v t) = Forall (Metadata si) v t

rowToList :: Type -> [(Identifier, Type)]
rowToList (RExtension _ label type' row) = (label, type') : rowToList row
rowToList _ = []

rowFromList :: [(Identifier, Type)] -> Type
rowFromList ((label, type') : fields) = RExtension (Metadata Missing) label type' (rowFromList fields)
rowFromList _ = REmpty (Metadata Missing)

getLeafRow :: Type -> Type
getLeafRow (RExtension _ _ _ row) = getLeafRow row
getLeafRow e = e

-- | Converts a polymorphic 'Type' to a monomorphic 'Mono.Type'
-- and fails if there's any 'TVar' in the type.
toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TAny si) = Right (Mono.TAny si)
toMonomorphic (TTuple si f s r) =
  Mono.TTuple si <$> toMonomorphic f
                 <*> toMonomorphic s
                 <*> mapM toMonomorphic r
toMonomorphic (TVar _ _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon si n) = Right $ Mono.TCon si n
toMonomorphic (TNoArgFn si t) = Mono.TNoArgFn si <$> toMonomorphic t
toMonomorphic (TFn si tx ty) = Mono.TFn si <$> toMonomorphic tx
                                           <*> toMonomorphic ty
toMonomorphic (TUncurriedFn si a rs) =
  Mono.TUncurriedFn si <$> mapM toMonomorphic a
                       <*> mapM toMonomorphic rs
toMonomorphic (TVariadicFn si a v rs) =
  Mono.TVariadicFn si <$> mapM toMonomorphic a
                      <*> toMonomorphic v
                      <*> mapM toMonomorphic rs
toMonomorphic (TSlice si t) = Mono.TSlice si <$> toMonomorphic t
toMonomorphic (TRecord si row) = Mono.TRecord si <$> toMonomorphic row
toMonomorphic (TNamed si n t) = Mono.TNamed si n <$> toMonomorphic t
toMonomorphic (REmpty si) = return (Mono.REmpty si)
toMonomorphic (RExtension si label polyType row) =
  Mono.RExtension si label <$> toMonomorphic polyType <*> toMonomorphic row

-- | Predicate returning if there's any 'TVar' in the 'Scheme'.
isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall _ tvars _) = not (null tvars)

-- TODO: Use 'toMonomorphic' here and check if it's a Left or Right value?
-- | Predicate returning if there's any 'TVar' in the 'Type'.
isPolymorphicType :: Type -> Bool
isPolymorphicType TAny{} = False
isPolymorphicType (TTuple _ f s r) = any isPolymorphicType (f:s:r)
isPolymorphicType (TVar _ _) = True
isPolymorphicType TCon{} = False
isPolymorphicType (TNoArgFn _ a) = isPolymorphicType a
isPolymorphicType (TFn _ a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TUncurriedFn _ a r) =
  any isPolymorphicType a || any isPolymorphicType r
isPolymorphicType (TVariadicFn _ a v r) =
  any isPolymorphicType a || isPolymorphicType v || any isPolymorphicType r
isPolymorphicType (TSlice _ a) = isPolymorphicType a
isPolymorphicType (TRecord _ r) = isPolymorphicType r
isPolymorphicType (TNamed _ _ t) = isPolymorphicType t
isPolymorphicType (REmpty _) = False
isPolymorphicType (RExtension _ _ type' row) =
  isPolymorphicType type' || isPolymorphicType row

underlying :: Type -> Type
underlying (TNamed _ _ t) = underlying t
underlying t = t

-- | Values that can return the free type variables they contain.
class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv TAny{}                     = Set.empty
  ftv (TTuple _ f s r)           = ftv (f:s:r)
  ftv TCon{}                     = Set.empty
  ftv (TVar _ a)                 = Set.singleton a
  ftv (TFn _ t1 t2)              = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn _ t)             = ftv t
  ftv (TUncurriedFn _ as rs)     = ftv (as ++ rs)
  ftv (TVariadicFn _ as v rs)    = ftv (v:(as ++ rs))
  ftv (TSlice _ t)               = ftv t
  ftv (TRecord _ r)              = ftv r
  ftv (TNamed _ _ t)             = ftv t
  ftv (REmpty _)                 = Set.empty
  ftv (RExtension _ _ type' row) = ftv type' `Set.union` ftv row

instance FTV Scheme where
  ftv (Forall _ as t) =
    ftv t `Set.difference` Set.fromList (map getBindingVar as)

instance FTV a => FTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty
