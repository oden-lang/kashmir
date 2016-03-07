-- | Instantiates polymorphically typed expressions with monomorphic types.
module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map              as Map hiding (foldl)

import qualified Oden.Core             as Core
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data InstantiateError = TypeMismatch SourceInfo Poly.Type Mono.Type
                      | SubstitutionFailed SourceInfo Poly.TVar [Poly.TVar]
                      deriving (Show, Eq, Ord)

type Substitutions = Map.Map Poly.TVar Poly.Type
type Instantiate a = StateT Substitutions (Except InstantiateError) a

monoToPoly :: Mono.Type -> Poly.Type
monoToPoly (Mono.TAny si) = Poly.TAny si
monoToPoly (Mono.TUnit si) = Poly.TUnit si
monoToPoly (Mono.TBasic si b) = Poly.TBasic si b
monoToPoly (Mono.TTuple si f s r) = Poly.TTuple si (monoToPoly f) (monoToPoly s) (map monoToPoly r)
monoToPoly (Mono.TCon si d r) = Poly.TCon si (monoToPoly d) (monoToPoly r)
monoToPoly (Mono.TNoArgFn si f) = Poly.TNoArgFn si (monoToPoly f)
monoToPoly (Mono.TFn si f p) = Poly.TFn si (monoToPoly f) (monoToPoly p)
monoToPoly (Mono.TUncurriedFn si as r) = Poly.TUncurriedFn si (map monoToPoly as) (map monoToPoly r)
monoToPoly (Mono.TVariadicFn si as v r) = Poly.TVariadicFn si (map monoToPoly as) (monoToPoly v) (map monoToPoly r)
monoToPoly (Mono.TSlice si t) = Poly.TSlice si (monoToPoly t)
monoToPoly (Mono.TStruct si fs) = Poly.TStruct si (map fieldMonoToPoly fs)
  where fieldMonoToPoly (Mono.TStructField si' n mt) =
          Poly.TStructField si' n (monoToPoly mt)
monoToPoly (Mono.TNamed si n t) = Poly.TNamed si n (monoToPoly t)

getSubstitutions :: Poly.Type -> Mono.Type -> Either InstantiateError Substitutions
getSubstitutions p@(Poly.TBasic _ pb) m@(Mono.TBasic _ mb) =
  if pb == mb
  then Right Map.empty
  else Left (TypeMismatch (getSourceInfo m) p m)
getSubstitutions (Poly.TCon _ pd pr) (Mono.TCon _ md mr) = do
  ds <- getSubstitutions pd md
  rs <- getSubstitutions pr mr
  return (ds `mappend` rs)
getSubstitutions (Poly.TNoArgFn _ pf) (Mono.TNoArgFn _ mf) =
  getSubstitutions pf mf
getSubstitutions (Poly.TFn _ pf pp) (Mono.TFn _ mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (Poly.TVar _ v) mono = Right (Map.singleton v (monoToPoly mono))
getSubstitutions (Poly.TUncurriedFn _ pas prs) (Mono.TUncurriedFn _ mas mrs) = do
  as <- zipWithM getSubstitutions pas mas
  rs <- zipWithM getSubstitutions prs mrs
  return (mconcat (rs ++ as))
getSubstitutions (Poly.TVariadicFn _ pas pv prs) (Mono.TVariadicFn _ mas mv mrs) = do
  as <- zipWithM getSubstitutions pas mas
  rs <- zipWithM getSubstitutions prs mrs
  v <- getSubstitutions pv mv
  return (mconcat (v:(as ++ rs)))
getSubstitutions (Poly.TTuple _ pf ps pr) (Mono.TTuple _ mf ms mr) = do
  f <- getSubstitutions pf mf
  s <- getSubstitutions ps ms
  r <- zipWithM getSubstitutions pr mr
  return (foldl mappend f (s:r))
getSubstitutions (Poly.TSlice _ p) (Mono.TSlice _ m) =
  getSubstitutions p m
getSubstitutions poly mono = Left (TypeMismatch (getSourceInfo mono) poly mono)

replace :: Poly.Type -> Instantiate Poly.Type
replace (Poly.TAny si) = return (Poly.TAny si)
replace (Poly.TUnit si) = return (Poly.TUnit si)
replace (Poly.TBasic si b) = return (Poly.TBasic si b)
replace (Poly.TTuple si f s r) =
  Poly.TTuple si <$> replace f <*> replace s <*> mapM replace r
replace (Poly.TVar si v) = do
  s <- get
  case Map.lookup v s of
    Just mono -> return (setSourceInfo si mono)
    Nothing -> throwError (SubstitutionFailed si v (Map.keys s))
replace (Poly.TCon si d r) = Poly.TCon si <$> replace d <*> replace r
replace (Poly.TNoArgFn si t) = Poly.TNoArgFn si <$> replace t
replace (Poly.TFn si ft pt) = Poly.TFn si <$> replace ft <*> replace pt
replace (Poly.TUncurriedFn si ft pt) =
  Poly.TUncurriedFn si <$> mapM replace ft <*> mapM replace pt
replace (Poly.TVariadicFn si ft vt pt) =
  Poly.TVariadicFn si <$> mapM replace ft <*> replace vt <*> mapM replace pt
replace (Poly.TSlice si t) = Poly.TSlice si <$> replace t
replace (Poly.TStruct si fs) = Poly.TStruct si <$> mapM replaceField fs
  where replaceField (Poly.TStructField si' n t) =
          Poly.TStructField si' n <$> replace t
replace (Poly.TNamed si n t) = Poly.TNamed si n <$> replace t

instantiateExpr :: Core.Expr Poly.Type
                -> Instantiate (Core.Expr Poly.Type)
instantiateExpr (Core.Symbol si i t) =
  Core.Symbol si i <$> replace t
instantiateExpr (Core.Subscript si s i t) =
  Core.Subscript si <$> instantiateExpr s
                    <*> instantiateExpr i
                    <*> replace t

instantiateExpr (Core.Subslice si s (Core.Range e1 e2) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.Range <$> instantiateExpr e1 <*> instantiateExpr e2)
                   <*> replace t
instantiateExpr (Core.Subslice si s (Core.RangeTo e) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.RangeTo <$> instantiateExpr e)
                   <*> replace t
instantiateExpr (Core.Subslice si s (Core.RangeFrom e) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.RangeFrom <$> instantiateExpr e)
                   <*> replace t

instantiateExpr (Core.UnaryOp si o e t) =
  Core.UnaryOp si o <$> instantiateExpr e <*> replace t
instantiateExpr (Core.BinaryOp si o e1 e2 t) =
  Core.BinaryOp si o <$> instantiateExpr e1
                     <*> instantiateExpr e2
                     <*> replace t
instantiateExpr (Core.Application si f p t) =
  Core.Application si <$> instantiateExpr f
                      <*> instantiateExpr p
                      <*> replace t
instantiateExpr (Core.NoArgApplication si f t) =
  Core.NoArgApplication si <$> instantiateExpr f
                           <*> replace t
instantiateExpr (Core.UncurriedFnApplication si f ps t) =
  Core.UncurriedFnApplication si <$> instantiateExpr f
                                 <*> mapM instantiateExpr ps
                                 <*> replace t
instantiateExpr (Core.Fn si a b t) =
  Core.Fn si a <$> instantiateExpr b
               <*> replace t
instantiateExpr (Core.NoArgFn si b t) =
  Core.NoArgFn si <$> instantiateExpr b
                  <*> replace t
instantiateExpr (Core.Let si name e b t) =
  Core.Let si name <$> instantiateExpr e
                   <*> instantiateExpr b
                   <*> replace t
instantiateExpr (Core.Literal si l t) =
  Core.Literal si l <$> replace t
instantiateExpr (Core.If si c tb eb t) =
  Core.If si <$> instantiateExpr c
             <*> instantiateExpr tb
             <*> instantiateExpr eb
             <*> replace t
instantiateExpr (Core.Tuple si fe se rs t) =
  Core.Tuple si <$> instantiateExpr fe
                <*> instantiateExpr se
                <*> mapM instantiateExpr rs
                <*> replace t
instantiateExpr (Core.Slice si es t) =
  Core.Slice si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (Core.Block si es t) =
  Core.Block si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (Core.StructInitializer si t vs) =
  Core.StructInitializer si <$> replace t <*> mapM instantiateExpr vs

-- | Given a polymorphically typed expression and a monomorphic type, return
-- the expression with all types substitued for monomorphic ones. If there's
-- a mismatch an error is thrown.
instantiate :: Core.Expr Poly.Type
            -> Mono.Type
            -> Either InstantiateError (Core.Expr Poly.Type)
instantiate expr mono = do
  subst <- getSubstitutions (Core.typeOf expr) mono
  (expr', _) <- runExcept (runStateT (instantiateExpr expr) subst)
  return expr'
