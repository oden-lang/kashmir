module Oden.Infer.Subsumption (
  SubsumptionError(..),
  subsumedBy,
  collectSubstitutions
) where

import Oden.Core as Core
import Oden.Infer.Substitution
import Oden.Metadata
import Oden.Type.Kind
import Oden.Type.Polymorphic
import Oden.SourceInfo

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except

import qualified Data.Map               as Map

data SubsumptionError = SubsumptionError SourceInfo Type Type
                      deriving (Show, Eq)

type Subsume a = StateT (Map.Map TVar Type) (Except SubsumptionError) a

-- | Collects the substitutions in the 'Subsume' state for matching types and
-- throws 'SubsumptionError' on mismatches.
collectSubstitutions :: Type -> Type -> Subsume ()
collectSubstitutions t1 (TNamed _ _ t2) = collectSubstitutions t1 t2
collectSubstitutions (TNamed _ _ t1) t2 = collectSubstitutions t1 t2
collectSubstitutions (TCon _ n1) (TCon _ n2)
  | n1 == n2 = return ()
collectSubstitutions t (TVar (Metadata si) tv) = do
  st <- gets (Map.lookup tv)
  case st of
    Just t' | t == t'   -> return ()
            | otherwise -> throwError (SubsumptionError si t t')
    Nothing -> modify (Map.insert tv t)
collectSubstitutions (TFn _ a1 r1) (TFn _ a2 r2) = do
  collectSubstitutions a1 a2
  collectSubstitutions r1 r2
collectSubstitutions (TNoArgFn _ r1) (TNoArgFn _ r2) = collectSubstitutions r1 r2
collectSubstitutions (TUncurriedFn _ a1 r1) (TUncurriedFn _ a2 r2) =
  mapM_ (uncurry collectSubstitutions) ((zip r1 r2) ++ (zip a1 a2))
collectSubstitutions (TVariadicFn _ a1 v1 r1) (TVariadicFn _ a2 v2 r2) =
  mapM_ (uncurry collectSubstitutions) ((v1, v2) : ((zip a1 a2) ++ (zip r1 r2)))
collectSubstitutions (TSlice _ t1) (TSlice _ t2) = collectSubstitutions t1 t2
collectSubstitutions (TTuple _ f1 s1 r1) (TTuple _ f2 s2 r2) = do
  collectSubstitutions f1 f2
  collectSubstitutions s1 s2
  zipWithM_ collectSubstitutions r1 r2
collectSubstitutions TAny{} _ = return ()
collectSubstitutions (TRecord _ r1) (TRecord _ r2) =
  collectSubstitutions r1 r2
collectSubstitutions r1 r2 | kindOf r1 == Row && kindOf r2 == Row = do
  let f1 = rowToList r1
      f2 = rowToList r2
  mapM_ (collectFromFields (Map.fromList f2)) f1
  where
  collectFromFields general (label, type') =
    case Map.lookup label general of
      Just genType -> collectSubstitutions type' genType
      Nothing -> throwError (SubsumptionError (getSourceInfo r2) r1 r2)
collectSubstitutions t1 t2 = throwError (SubsumptionError (getSourceInfo t2) t1 t2)

-- | Test if a type scheme is subsumed by an expression with a more general
-- type. If so, return the expression specialized to the less general type (all
-- subexpression types being substituted as well).
subsumedBy :: Scheme -> Core.Expr Type -> Either SubsumptionError Core.CanonicalExpr
subsumedBy s@(Forall _ _ st) expr = do
  subst <- snd <$> runExcept (runStateT (collectSubstitutions st (Core.typeOf expr)) Map.empty)
  return (s, apply (Subst subst) expr)
