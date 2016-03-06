-- | This module contains values representing polymorphic types, i.e. types
-- that can be instantiated into other polymorphic types and purely monomorphic
-- types.
module Oden.Type.Polymorphic (
  TVar(..),
  StructField(..),
  getStructFieldType,
  Type(..),
  TVarBinding(..),
  Scheme(..),
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  FTV,
  ftv,
  getBindingVar,
  equalsT,
  underlying
) where

import           Oden.Type.Basic
import qualified Oden.Type.Monomorphic as Mono
import           Oden.SourceInfo
import           Oden.QualifiedName

import qualified Data.Set               as Set

-- | Wrapper for type variable names.
newtype TVar = TV String
  deriving (Show, Eq, Ord)

-- | The name and type of a struct field.
data StructField = TStructField SourceInfo String Type
                 deriving (Show, Eq, Ord)

getStructFieldType :: StructField -> Type
getStructFieldType (TStructField _ _ t) = t

-- | A polymorphic type.
data Type
  = TAny SourceInfo
  -- | A type variable.
  | TBasic SourceInfo BasicType
  -- | A type variable.
  | TVar SourceInfo TVar
  -- | The unit type.
  | TUnit SourceInfo
  -- | A tuple, with at least two elements.
  | TTuple SourceInfo Type Type [Type]
  -- | A type constructor.
  | TCon SourceInfo Type Type
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn SourceInfo Type
  -- | A curried function.
  | TFn SourceInfo Type Type
  -- | A slice type.
  | TSlice SourceInfo Type
  -- | Data structure type.
  | TStruct SourceInfo [StructField]
  -- | A name for a type, introduced by type definitions.
  | TNamed SourceInfo QualifiedName Type

  -- For foreign definitions:

  -- | A function that can have multiple arguments (no currying).
  | TUncurriedFn SourceInfo [Type] [Type]
  | TVariadicFn SourceInfo [Type] Type Type -- TODO: Support multiple return values somehow.
  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny si)              = si
  getSourceInfo (TBasic si _)          = si
  getSourceInfo (TUnit si)             = si
  getSourceInfo (TVar si _)            = si
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
  setSourceInfo si (TVar _ v)            = TVar si v
  setSourceInfo si (TTuple _ f s r)      = TTuple si f s r
  setSourceInfo si (TFn _ a r)           = TFn si a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn si r
  setSourceInfo si (TCon _ d r)          = TCon si d r
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn si a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn si a v r
  setSourceInfo si (TSlice _ t)          = TSlice si t
  setSourceInfo si (TStruct _ fs)        = TStruct si fs
  setSourceInfo si (TNamed _ n t)        = TNamed si n t

-- | A type variable binding.
data TVarBinding = TVarBinding SourceInfo TVar
                 deriving (Show, Eq, Ord)

instance FTV TVarBinding where
  ftv (TVarBinding _ tv) = Set.singleton tv

getBindingVar :: TVarBinding -> TVar
getBindingVar (TVarBinding _ v)  = v

instance HasSourceInfo TVarBinding where
  getSourceInfo (TVarBinding si _)   = si
  setSourceInfo si (TVarBinding _ v) = TVarBinding si v

-- | A polymorphic type and its quantified type variable bindings.
data Scheme = Forall SourceInfo [TVarBinding] Type
            deriving (Show, Eq, Ord)

instance HasSourceInfo Scheme where
  getSourceInfo (Forall si _ _) = si
  setSourceInfo si (Forall _ v t) = Forall si v t

-- | Converts a polymorphic 'Type' to a monomorphic 'Mono.Type'
-- and fails if there's any 'TVar' in the type.
toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TAny si) = Right (Mono.TAny si)
toMonomorphic (TBasic si b) = Right (Mono.TBasic si b)
toMonomorphic (TUnit si) = Right (Mono.TUnit si)
toMonomorphic (TTuple si f s r) =
  Mono.TTuple si <$> toMonomorphic f
                 <*> toMonomorphic s
                 <*> mapM toMonomorphic r
toMonomorphic (TVar _ _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon si d r) = Mono.TCon si <$> toMonomorphic d <*> toMonomorphic r
toMonomorphic (TNoArgFn si t) = Mono.TNoArgFn si <$> toMonomorphic t
toMonomorphic (TFn si tx ty) = Mono.TFn si <$> toMonomorphic tx
                                           <*> toMonomorphic ty
toMonomorphic (TUncurriedFn si a rs) =
  Mono.TUncurriedFn si <$> mapM toMonomorphic a
                       <*> mapM toMonomorphic rs
toMonomorphic (TVariadicFn si a v r) =
  Mono.TVariadicFn si <$> mapM toMonomorphic a
                      <*> toMonomorphic v
                      <*> toMonomorphic r
toMonomorphic (TSlice si t) = Mono.TSlice si <$> toMonomorphic t
toMonomorphic (TStruct si fs) = Mono.TStruct si <$> mapM toMonomorphicStructField fs
  where toMonomorphicStructField (TStructField si' n pt) =
          Mono.TStructField si' n <$> toMonomorphic pt
toMonomorphic (TNamed si n t) = Mono.TNamed si n <$> toMonomorphic t

-- | Predicate returning if there's any 'TVar' in the 'Scheme'.
isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall _ tvars _) = not (null tvars)

-- TODO: Use 'toMonomorphic' here and check if it's a Left or Right value?
-- | Predicate returning if there's any 'TVar' in the 'Type'.
isPolymorphicType :: Type -> Bool
isPolymorphicType TAny{} = False
isPolymorphicType TUnit{} = False
isPolymorphicType TBasic{} = False
isPolymorphicType (TTuple _ f s r) = any isPolymorphicType (f:s:r)
isPolymorphicType (TVar _ _) = True
isPolymorphicType (TCon _ d r) = isPolymorphicType d || isPolymorphicType r
isPolymorphicType (TNoArgFn _ a) = isPolymorphicType a
isPolymorphicType (TFn _ a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TUncurriedFn _ a r) =
  any isPolymorphicType a || any isPolymorphicType r
isPolymorphicType (TVariadicFn _ a v r) =
  any isPolymorphicType a || isPolymorphicType v || isPolymorphicType r
isPolymorphicType (TSlice _ a) = isPolymorphicType a
isPolymorphicType (TStruct _ fs) = any (isPolymorphicType . getStructFieldType) fs
isPolymorphicType (TNamed _ _ t) = isPolymorphicType t

-- | Equality for '[Type]', disregarding SourceInfo.
equalsAllT :: [Type] -> [Type] -> Bool
equalsAllT t1 t2 = and (zipWith equalsT t1 t2)

-- | Equality for 'Type', disregarding SourceInfo.
equalsT :: Type -> Type -> Bool
equalsT TAny{} TAny{} = True
equalsT TUnit{} TUnit{} = True
equalsT (TBasic _ b1) (TBasic _ b2) = b1 == b2
equalsT (TTuple _ f1 s1 r1) (TTuple _ f2 s2 r2) =
  f1 `equalsT` f2 && s1 `equalsT` s2 && r1 `equalsAllT` r2
equalsT (TVar _ v1) (TVar _ v2) = v1 == v2
equalsT (TCon _ d1 r1) (TCon _ d2 r2) = d1 `equalsT` d2 && r1 `equalsT` r2
equalsT (TNoArgFn _ a1) (TNoArgFn _ a2) = a1 `equalsT` a2
equalsT (TFn _ a1 b1) (TFn _ a2 b2) = a1 `equalsT` a2 && b1 `equalsT` b2
equalsT (TUncurriedFn _ a1 r1) (TUncurriedFn _ a2 r2) =
  a1 `equalsAllT` a2 && r1 `equalsAllT` r2
equalsT (TVariadicFn _ a1 v1 r1) (TVariadicFn _ a2 v2 r2)=
  a1 `equalsAllT` a2 && v1 `equalsT` v2 && r1 `equalsT` r2
equalsT (TSlice _ e1) (TSlice _ e2) = e1 `equalsT` e2
equalsT (TStruct _ fs1) (TStruct _ fs2) =
  map getStructFieldType fs1 `equalsAllT` map getStructFieldType fs2
equalsT (TNamed _ n1 t1) (TNamed _ n2 t2) =
  n1 == n2 && t1 `equalsT` t2
equalsT _ _ = False

underlying :: Type -> Type
underlying (TNamed _ _ t) = underlying t
underlying t = t

-- | Values that can return the free type variables they contain.
class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv TAny{}                    = Set.empty
  ftv TBasic{}                  = Set.empty
  ftv TUnit{}                   = Set.empty
  ftv (TTuple _ f s r)          = ftv (f:s:r)
  ftv (TCon _ d r)              = ftv d `Set.union` ftv r
  ftv (TVar _ a)                = Set.singleton a
  ftv (TFn _ t1 t2)             = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn _ t)            = ftv t
  ftv (TUncurriedFn _ as r)     = ftv (r ++ as)
  ftv (TVariadicFn _ as v r)    = ftv (v:r:as)
  ftv (TSlice _ t)              = ftv t
  ftv (TStruct _ fs)            = ftv (map getStructFieldType fs)
  ftv (TNamed _ _ t)            = ftv t

instance FTV Scheme where
  ftv (Forall _ as t) =
    ftv t `Set.difference` Set.fromList (map getBindingVar as)

instance FTV a => FTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty
