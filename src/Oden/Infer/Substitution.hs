{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Oden.Infer.Substitution where

import Oden.Type.Polymorphic
import Oden.Core as Core

import qualified Data.Set               as Set
import qualified Data.Map               as Map

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Union of substitutions
union :: Subst -> Subst -> Subst
(Subst s1) `union` (Subst s2) = Subst (s1 `Map.union` s2)

class FTV a => Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply _ (TAny si)               = TAny si
  apply _ (TUnit si)              = TUnit si
  apply s (TTuple si f s' r)      = TTuple si (apply s f) (apply s s') (apply s r)
  apply _ (TBasic si b)           = TBasic si b
  apply s (TCon si a ts)          = TCon si a (apply s ts)
  apply (Subst s) t@(TVar _ a)    = Map.findWithDefault t a s
  apply s (TNoArgFn si t)         = TNoArgFn si (apply s t)
  apply s (TFn si t1 t2)          = TFn si (apply s t1) (apply s t2)
  apply s (TUncurriedFn si as r)  = TUncurriedFn si (map (apply s) as) (apply s r)
  apply s (TVariadicFn si as v r) = TVariadicFn si (map (apply s) as) (apply s v) (apply s r)
  apply s (TSlice si t)           = TSlice si (apply s t)

instance Substitutable Scheme where
  apply (Subst s) (Forall si as t) =
    Forall si as (apply s' t)
    where s' = Subst $ foldr (Map.delete . getBindingVar) s as

instance FTV Core.CanonicalExpr where
  ftv (sc, expr) = ftv sc `Set.union` ftv expr

instance Substitutable Core.CanonicalExpr where
  apply s (sc, expr) = (apply s sc, apply s expr)

instance FTV (Core.Expr Type) where
  ftv = ftv . Core.typeOf

instance Substitutable (Core.Expr Type) where
  apply s (Core.Symbol si x t)                   = Core.Symbol si x (apply s t)
  apply s (Core.Subscript si es i t)             = Core.Subscript si (apply s es) (apply s i) (apply s t)
  apply s (Core.Subslice si es i1 i2 t)          = Core.Subslice si (apply s es) (fmap (apply s) i1) (fmap (apply s) i2) (apply s t)
  apply s (Core.UnaryOp si o e t)                = Core.UnaryOp si o (apply s e) (apply s t)
  apply s (Core.BinaryOp si o e1 e2 t)           = Core.BinaryOp si o (apply s e1) (apply s e2) (apply s t)
  apply s (Core.Application si f p t)            = Core.Application si (apply s f) (apply s p) (apply s t)
  apply s (Core.NoArgApplication si f t)         = Core.NoArgApplication si (apply s f) (apply s t)
  apply s (Core.UncurriedFnApplication si f p t) = Core.UncurriedFnApplication si (apply s f) (apply s p) (apply s t)
  apply s (Core.Fn si x b t)                     = Core.Fn si x (apply s b) (apply s t)
  apply s (Core.NoArgFn si b t)                  = Core.NoArgFn si (apply s b) (apply s t)
  apply s (Core.Let si x e b t)                  = Core.Let si x (apply s e) (apply s b) (apply s t)
  apply s (Core.Literal si l t)                  = Core.Literal si l (apply s t)
  apply s (Core.Tuple si fe se re t)             = Core.Tuple si (apply s fe) (apply s se) (apply s re) (apply s t)
  apply s (Core.If si c tb fb t)                 = Core.If si (apply s c) (apply s tb) (apply s fb) (apply s t)
  apply s (Core.Slice si es t)                   = Core.Slice si (apply s es) (apply s t)
  apply s (Core.Block si es t)                   = Core.Block si (apply s es) (apply s t)

instance Substitutable a => Substitutable [a] where
  apply = map . apply
