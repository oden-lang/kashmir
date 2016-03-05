{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Oden.Infer (
  TypeError(..),
  Subst(..),
  TypeBinding(..),
  TypingEnvironment,
  inferExpr,
  inferDefinition,
  inferPackage,
  constraintsExpr
) where

import           Control.Arrow           (left)
import           Control.Monad.Except
import           Control.Monad.RWS       hiding ((<>))

import qualified Data.Map                as Map
import qualified Data.Set                as Set

import qualified Oden.Core               as Core
import           Oden.Core.Operator
import qualified Oden.Core.Untyped       as Untyped
import           Oden.Environment        as Environment hiding (map)
import           Oden.Identifier
import           Oden.Infer.Environment
import           Oden.Infer.Substitution
import           Oden.Infer.Subsumption
import           Oden.Infer.Unification
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName      (QualifiedName(..), nameInUniverse)
import           Oden.SourceInfo
import           Oden.Type.Polymorphic
import           Oden.Type.Signature

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad.
type Infer a = (RWST
                  TypingEnvironment -- Typing environment
                  [Constraint]              -- Generated constraints
                  InferState                -- Inference state
                  (Except                   -- Inference errors
                    TypeError)
                  a)                        -- Result

-- | Inference state.
data InferState = InferState { count :: Int }

-- | Initial inference state.
initInfer :: InferState
initInfer = InferState { count = 0 }

instance FTV TypeBinding where
  ftv (Package _ _ e)        = ftv e
  ftv (Local _ _ d)          = ftv d
  ftv (Type _ _ _ fs)        = ftv fs
  ftv (QuantifiedType _ _ t) = ftv t

instance Substitutable TypeBinding where
  apply _ (Package si n e)        = Package si n e
  apply s (Local si n d)          = Local si n (apply s d)
  apply s (Type si n bs t)        = Type si n bs (apply s t)
  apply s (QuantifiedType si n t) = QuantifiedType si n (apply s t)

instance FTV TypingEnvironment where
  ftv (Environment env) = ftv $ Map.elems env

instance Substitutable TypingEnvironment where
  apply s (Environment env) = Environment $ Map.map (apply s) env

data TypeError
  = UnificationError UnificationError
  | PackageNotInScope SourceInfo Identifier
  | NotInScope SourceInfo Identifier
  | MemberNotInPackage SourceInfo Identifier Identifier
  | ArgumentCountMismatch (Core.Expr Type) [Type] [Type]
  | TypeSignatureSubsumptionError Identifier SubsumptionError
  | InvalidPackageReference SourceInfo Identifier
  | ValueUsedAsType SourceInfo Identifier
  | TypeIsNotAnExpression SourceInfo Identifier
  deriving (Show, Eq)

-- | Run the inference monad.
runInfer :: TypingEnvironment -> Infer a -> Either TypeError (a, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the top-level type of an expression in a given typing
-- environment.
inferExpr :: TypingEnvironment
          -> Untyped.Expr
          -> Either TypeError Core.CanonicalExpr
inferExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- left UnificationError $ runSolve cs
  return $ closeOver (apply subst te)

-- | Return the internal constraints used in solving for the type of an
-- expression.
constraintsExpr :: TypingEnvironment
                -> Untyped.Expr
                -> Either TypeError ([Constraint], Subst, Core.Expr Type, Scheme)
constraintsExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- left UnificationError $ runSolve cs
  let (sc, te') = closeOver $ apply subst te
  return (cs, subst, te', sc)

-- | Canonicalize and return the polymorphic top-level type.
closeOver :: Core.Expr Type -> Core.CanonicalExpr
closeOver = normalize . generalize empty

-- | Unify two types. Order matters in some cases as the first type is the one
-- being subsumed by the second, e.g. when unifying with TAny.
uni :: SourceInfo -> Type -> Type -> Infer ()
uni si t1 t2 = tell [(si, t1, t2)]

-- | Extend the typing environment.
inEnv :: (Identifier, TypeBinding) -> Infer a -> Infer a
inEnv (x, sc) = local (`extend` (x, sc))

lookupTypeIn :: TypingEnvironment -> Metadata SourceInfo -> Identifier -> Infer Type
lookupTypeIn _ si (Identifier "any") = return (TAny si)
lookupTypeIn env (Metadata si) identifier =
  case Environment.lookup identifier env of
    Nothing                     -> throwError $ NotInScope si identifier
    Just Package{}              -> throwError $ InvalidPackageReference si identifier
    Just Local{}                -> throwError $ ValueUsedAsType si identifier
    Just (Type _ _ _ t)         -> return t
    Just (QuantifiedType _ _ t) -> return t

-- | Lookup a type in the environment.
lookupType :: Metadata SourceInfo -> Identifier -> Infer Type
lookupType si identifier = do
  env <- ask
  lookupTypeIn env si identifier

lookupValueIn :: TypingEnvironment -> Metadata SourceInfo -> Identifier -> Infer Type
lookupValueIn env (Metadata si) identifier = do
  type' <- case Environment.lookup identifier env of
            Nothing               -> throwError $ NotInScope si identifier
            Just Package{}        -> throwError $ InvalidPackageReference si identifier
            Just (Local _ _ sc)   -> instantiate sc
            Just Type{}           -> throwError (TypeIsNotAnExpression si identifier)
            Just QuantifiedType{} -> throwError (TypeIsNotAnExpression si identifier)
  return $ setSourceInfo si type'

-- | Lookup type of a value in the environment.
lookupValue :: Metadata SourceInfo -> Identifier -> Infer Type
lookupValue si identifier = do
  env <- ask
  lookupValueIn env si identifier
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Create a new type variable with a unique name.
fresh :: Metadata SourceInfo -> Infer Type
fresh si = do
    s <- get
    put s{count = count s + 1}
    return $ TVar si (TV ("_t" ++ show (count s)))

-- | Create a type based on scheme but with all fresh type variables.
instantiate :: Scheme -> Infer Type
instantiate (Forall _ as t) = do
    as' <- mapM (fresh . Metadata . getSourceInfo) as
    let s = Subst $ Map.fromList $ zip (map getBindingVar as) as'
    return $ apply s t

-- | Given a typed expression, return a canonical expression with the free
-- type variables (not present in the environment) declared as type variable
-- bindings for the expression.
generalize :: TypingEnvironment -> Core.Expr Type -> Core.CanonicalExpr
generalize env expr  = (Forall (Metadata $ getSourceInfo expr) bindings (Core.typeOf expr), expr)
    where bindings = map (TVarBinding $ Metadata Missing) (Set.toList $ ftv expr `Set.difference` ftv env)

universeType :: Metadata SourceInfo -> String -> Type
universeType si n = TCon si (nameInUniverse n)

-- | The heart of the inferencer. Takes an untyped expression and returns the
-- inferred typed expression. Constraints are collected in the 'Infer' monad
-- and substitutions are made before the inference is complete, so the
-- expressions returned from 'infer' are not the final results.
infer :: Untyped.Expr -> Infer (Core.Expr Type)
infer expr = case expr of
  Untyped.Literal si Untyped.Unit ->
    return (Core.Literal si Core.Unit (TCon si (nameInUniverse "unit")))
  Untyped.Literal si (Untyped.Int n) ->
    return (Core.Literal si (Core.Int n) (TCon si (nameInUniverse "int")))
  Untyped.Literal si (Untyped.Bool b) ->
    return (Core.Literal si (Core.Bool b) (TCon si (nameInUniverse "bool")))
  Untyped.Literal si (Untyped.String s) ->
    return (Core.Literal si (Core.String s) (TCon si (nameInUniverse "string")))

  Untyped.Subscript si s i -> do
    st <- infer s
    it <- infer i
    tv <- fresh si
    uni (getSourceInfo st) (Core.typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
    uni (getSourceInfo it) (Core.typeOf it) (TCon si (nameInUniverse "int"))
    return (Core.Subscript si st it tv)

  Untyped.Subslice si s range ->
    case range of
      (Untyped.Range lowerExpr upperExpr) -> do
        st <- infer s
        lowerExprTyped <- infer lowerExpr
        upperExprTyped <- infer upperExpr
        tv <- fresh si
        uni (getSourceInfo st) (Core.typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
        uni (getSourceInfo upperExprTyped) (Core.typeOf lowerExprTyped) (universeType si "int")
        uni (getSourceInfo upperExprTyped) (Core.typeOf upperExprTyped) (universeType si "int")
        return (Core.Subslice si st (Core.Range lowerExprTyped upperExprTyped) (TSlice si tv))
      (Untyped.RangeTo upperExpr) -> inferUnboundedRange Core.RangeTo upperExpr
      (Untyped.RangeFrom lowerExpr) -> inferUnboundedRange Core.RangeFrom lowerExpr

    where
    inferUnboundedRange f boundExpr = do
      st <- infer s
      boundExprTyped <- infer boundExpr
      tv <- fresh si
      uni (getSourceInfo st) (Core.typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
      uni (getSourceInfo boundExprTyped) (Core.typeOf boundExprTyped) (universeType si "int")
      return (Core.Subslice si st (f boundExprTyped) (TSlice si tv))

  Untyped.UnaryOp si o e -> do
    rt <- case o of
              Positive   -> return (universeType si "int")
              Negative -> return (universeType si "int")
              Not    -> return (universeType si "bool")
    te <- infer e
    uni (getSourceInfo te) (Core.typeOf te) rt
    return (Core.UnaryOp si o te rt)

  Untyped.BinaryOp si o e1 e2 -> do
    (ot, rt) <- case o of
                    Add               -> return (universeType si "int", universeType si "int")
                    Subtract          -> return (universeType si "int", universeType si "int")
                    Multiply          -> return (universeType si "int", universeType si "int")
                    Divide            -> return (universeType si "int", universeType si "int")
                    Equals            -> do tv <- fresh si
                                            return (tv, universeType si "bool")
                    Concat            -> return (universeType si "string", universeType si "string")
                    LessThan          -> return (universeType si "int", universeType si "bool")
                    GreaterThan       -> return (universeType si "int", universeType si "bool")
                    LessThanEqual     -> return (universeType si "int", universeType si "bool")
                    GreaterThanEqual  -> return (universeType si "int", universeType si "bool")
                    And               -> return (universeType si "bool", universeType si "bool")
                    Or                -> return (universeType si "bool", universeType si "bool")
    te1 <- infer e1
    te2 <- infer e2
    uni (getSourceInfo te1) (Core.typeOf te1) ot
    uni (getSourceInfo te2) (Core.typeOf te2) ot
    return (Core.BinaryOp si o te1 te2 rt)

  Untyped.Symbol si x -> do
    t <- lookupValue si x
    return $ Core.Symbol si x t

  Untyped.MemberAccess si expr'@(Untyped.Symbol symbolSourceInfo name) memberName -> do
    env <- ask
    case Environment.lookup name env of
      Just (Package _ _ pkgEnv) -> do
        valueType <- lookupValueIn pkgEnv si memberName
        return (Core.PackageMemberAccess si name memberName valueType)
      Just _ -> inferRecordFieldAccess si expr' memberName
      Nothing -> throwError $ NotInScope (unwrap symbolSourceInfo) name

  Untyped.MemberAccess si expr' fieldName ->
    inferRecordFieldAccess si expr' fieldName

  Untyped.Fn si (Untyped.NameBinding bsi a) b -> do
    tv <- fresh bsi
    tb <- inEnv (a, Local bsi a (Forall bsi [] tv)) (infer b)
    return (Core.Fn si (Core.NameBinding bsi a) tb (TFn si tv (Core.typeOf tb)))

  Untyped.NoArgFn si f -> do
    tf <- infer f
    return (Core.NoArgFn si tf (TNoArgFn si (Core.typeOf tf)))

  Untyped.Application si f [] -> do
    tv <- fresh si
    tf <- infer f
    case Core.typeOf tf of
      t@TUncurriedFn{} -> do
        uni (getSourceInfo tf) t (TUncurriedFn (Metadata $ getSourceInfo tf) [] [tv])
        return (Core.UncurriedFnApplication si tf [] tv)
      -- No-param application of variadic function is automatically transformed
      -- to application of empty slice.
      t@(TVariadicFn _ [] variadicArg _) -> do
        uni (getSourceInfo tf) t (TVariadicFn (Metadata $ getSourceInfo tf) [] variadicArg [tv])
        return (Core.UncurriedFnApplication si tf [Core.Slice (Metadata $ getSourceInfo variadicArg) [] variadicArg] tv)
      TVariadicFn _ nonVariadicArgs _ _ ->
        throwError (ArgumentCountMismatch tf nonVariadicArgs [])
      t -> do
        uni (getSourceInfo tf) t (TNoArgFn (Metadata $ getSourceInfo tf) tv)
        return (Core.NoArgApplication si tf tv)

  Untyped.Application si f ps -> do
    tf <- infer f

    case Core.typeOf tf of
      -- Uncurried non-variadic functions with a single return value
      t@(TUncurriedFn _ _ [_]) -> do
        tv <- fresh si
        tps <- mapM infer ps
        uni (getSourceInfo tf) t (TUncurriedFn si (map Core.typeOf tps) [tv])
        return (Core.UncurriedFnApplication si tf tps tv)

      -- Uncurried non-variadic functions with multiple return values
      TUncurriedFn _ as (r1:r2:rs) -> do
        tv <- fresh si
        tps <- mapM infer ps
        uni (getSourceInfo tf) (TUncurriedFn (Metadata $ getSourceInfo tf) as [TTuple si r1 r2 rs])
                               (TUncurriedFn (Metadata $ getSourceInfo tf) (map Core.typeOf tps) [tv])
        return (Core.UncurriedFnApplication si tf tps tv)

      -- Uncurried variadic functions with a single return value
      t@(TVariadicFn _ nonVariadicTypes variadicType [_]) -> do
        tv <- fresh si
        nonVariadicParams <- mapM infer (take (length nonVariadicTypes) ps)
        variadicParams <- mapM infer (drop (length nonVariadicTypes) ps)
        let sliceSi = if null variadicParams then Missing else getSourceInfo (head variadicParams)
        let allParams = nonVariadicParams ++ [Core.Slice (Metadata sliceSi) variadicParams variadicType]
        uni (getSourceInfo tf) t (TVariadicFn (Metadata $ getSourceInfo tf) (map Core.typeOf nonVariadicParams) variadicType [tv])
        return (Core.UncurriedFnApplication si tf allParams tv)

      -- Uncurried variadic functions with multiple return values
      TVariadicFn _ nonVariadicTypes variadicType (r1:r2:rs) -> do
        tv <- fresh si
        nonVariadicParams <- mapM infer (take (length nonVariadicTypes) ps)
        variadicParams <- mapM infer (drop (length nonVariadicTypes) ps)
        let sliceSi = if null variadicParams then Missing else getSourceInfo (head variadicParams)
        let allParams = nonVariadicParams ++ [Core.Slice (Metadata sliceSi) variadicParams variadicType]
        uni (getSourceInfo tf)
            (TVariadicFn (Metadata $ getSourceInfo tf) nonVariadicTypes                    variadicType [TTuple si r1 r2 rs])
            (TVariadicFn (Metadata $ getSourceInfo tf) (map Core.typeOf nonVariadicParams) variadicType [tv])
        return (Core.UncurriedFnApplication si tf allParams tv)

      -- No-arg functions
      t | ps == [] -> do
        tv <- fresh si
        uni (getSourceInfo tf) t (TNoArgFn (Metadata $ getSourceInfo tf) tv)
        return (Core.NoArgApplication si tf tv)

      -- Everything else, i.e. functions with a single argument and one return value
      _ ->
        foldM app tf ps
        where
        app :: Core.Expr Type -> Untyped.Expr -> Infer (Core.Expr Type)
        app tf' p = do
          tv <- fresh si
          tp <- infer p
          uni (getSourceInfo tf) (Core.typeOf tf') (TFn (Metadata $ getSourceInfo tf) (Core.typeOf tp) tv)
          return (Core.Application si tf' tp tv)

  Untyped.Let si (Untyped.NameBinding bsi n) e b -> do
    te <- infer e
    tb <- inEnv (n, Local bsi n (Forall si [] (Core.typeOf te))) (infer b)
    return (Core.Let si (Core.NameBinding bsi n) te tb (Core.typeOf tb))

  Untyped.If si cond tr fl -> do
    tcond <- infer cond
    ttr <- infer tr
    tfl <- infer fl
    uni (getSourceInfo tcond) (Core.typeOf tcond) (universeType si "bool")
    uni (getSourceInfo ttr) (Core.typeOf ttr) (Core.typeOf tfl)
    return (Core.If si tcond ttr tfl (Core.typeOf ttr))

  Untyped.Tuple si f s r -> do
    tf <- infer f
    ts <- infer s
    tr <- mapM infer r
    let t = TTuple si (Core.typeOf tf) (Core.typeOf ts) (map Core.typeOf tr)
    return (Core.Tuple si tf ts tr t)

  Untyped.Slice si es -> do
    tv <- fresh si
    tes <- mapM infer es
    mapM_ (uni (unwrap si) tv . Core.typeOf) tes
    return (Core.Slice si tes (TSlice si tv))

  Untyped.Block si es -> do
    tv <- fresh si
    tes <- mapM infer es
    case tes of
      [] -> uni (unwrap si) tv (universeType si "unit")
      _ -> uni (unwrap si) tv (Core.typeOf (last tes))
    return (Core.Block si tes tv)

  Untyped.RecordInitializer si fields -> do
    (fieldInitializers, row) <- foldM unifyFields ([], REmpty si) fields
    return (Core.RecordInitializer si (TRecord si row) fieldInitializers)
    where
    unifyFields (typedFields, row) (Untyped.FieldInitializer fsi label expr') = do
      typedExpr <- infer expr'
      return (Core.FieldInitializer fsi label typedExpr : typedFields, RExtension fsi label (Core.typeOf typedExpr) row)

  where
  inferRecordFieldAccess si expr' label = do
    fieldType <- fresh si
    recordExtType <- fresh si
    typedExpr <- infer expr'
    uni (getSourceInfo typedExpr)
        (Core.typeOf typedExpr)
        (TRecord (Metadata $ getSourceInfo typedExpr) (RExtension si label fieldType recordExtType))
    return (Core.RecordFieldAccess si typedExpr label fieldType)

-- | Tries to resolve a user-supplied type expression to an actual type.
resolveType :: SignatureExpr SourceInfo -> Infer Type
resolveType (TSUnit si) = return (universeType (Metadata si) "unit")
resolveType (TSSymbol si i) = lookupType (Metadata si) i
resolveType (TSApp _si _e1 _e2) = error "Type constructor application not implemented yet."
resolveType (TSFn si de re) = TFn (Metadata si) <$> resolveType de <*> resolveType re
resolveType (TSNoArgFn si e) = TNoArgFn (Metadata si) <$> resolveType e
resolveType (TSTuple si fe se re) = TTuple (Metadata si) <$> resolveType fe
                                                         <*> resolveType se
                                                         <*> mapM resolveType re
resolveType (TSSlice si e) = TSlice (Metadata si) <$> resolveType e
resolveType (TSRowEmpty si) = return $ REmpty (Metadata si)
resolveType (TSRowExtension si label type' row) =
  RExtension (Metadata si) label <$> resolveType type'
                                 <*> resolveType row
resolveType (TSRecord si r) = TRecord (Metadata si) <$> resolveType r

-- | Tries to resolve a user-supplied type signature to an actual type scheme.
resolveTypeSignature :: TypeSignature SourceInfo -> Infer (Scheme, TypingEnvironment)
resolveTypeSignature (TypeSignature si bindings expr) = do
  env <- ask
  envWithBindings <- foldM extendWithBinding env bindings
  t <- local (const envWithBindings) (resolveType expr)
  return (Forall (Metadata si) (map toVarBinding bindings) t, envWithBindings)
  where
  extendWithBinding env' (SignatureVarBinding si' v) = do
    --return $ env' `extend` (v, QuantifiedType (Metadata si') v (TVar (Metadata si') (TV varName)))
    tv <- fresh (Metadata si')
    return $ env' `extend` (v, QuantifiedType (Metadata si') v tv)
  toVarBinding (SignatureVarBinding si' (Identifier v)) = TVarBinding (Metadata si') (TV v)

-- | Indicates if the canonical expression should be generated by closing over
-- the the free type variables in the inferred expression. This is done when
-- top type signatures are missing.
type ShouldCloseOver = Bool

-- | Infer the untyped definition in the Infer monad, returning a typed
-- version. Resolves type signatures of optionally type-annotated definitions.
inferDef :: Untyped.Definition -> Infer (Core.Definition, ShouldCloseOver)
inferDef (Untyped.Definition si name signature expr) = do
  env <- ask
  case signature of
    Nothing -> do
      tv <- fresh si
      let recScheme = Forall si [] tv
      let recursiveEnv = env `extend` (name, Local si name recScheme)
      te <- local (const recursiveEnv) (infer expr)
      return (Core.Definition si name (recScheme, te), True)
    Just ts -> do
      (recScheme@(Forall _ _ recType), envWithBindings) <- resolveTypeSignature ts
      let recursiveEnv = envWithBindings  `extend` (name, Local si name recScheme)
      te <- local (const recursiveEnv) (infer expr)
      uni (getSourceInfo te) recType (Core.typeOf te)
      case recScheme `subsumedBy` te of
        Left e -> throwError $ TypeSignatureSubsumptionError name e
        Right canonical -> return (Core.Definition si name canonical, False)

inferDef (Untyped.TypeDefinition si name params typeExpr) = do
  type' <- resolveType typeExpr
  return (Core.TypeDefinition si name (map convertParams params) type', False)
  where
  convertParams (Untyped.NameBinding bsi bn) = Core.NameBinding bsi bn

-- | Infer a top-level definitition, returning a typed version and the typing
-- environment extended with the definitions name and type.
inferDefinition :: TypingEnvironment -> Untyped.Definition -> Either TypeError (TypingEnvironment, Core.Definition)
inferDefinition env def = do
  -- Infer the definition.
  ((def', shouldCloseOver), cs) <- runInfer env (inferDef def)

  case def' of
    Core.Definition si name (_, te) | shouldCloseOver -> do
      subst <- left UnificationError $ runSolve cs
      let canonical'@(sc, _) = closeOver (apply subst te)
          env' = env `extend` (name, Local si name sc)
      return (env', Core.Definition si name canonical')
    Core.Definition si name canonical -> do
      subst <- left UnificationError $ runSolve cs
      let canonical'@(sc, _) = normalize (apply subst canonical)
          env' = env `extend` (name, Local si name sc)
      return (env', Core.Definition si name canonical')

    Core.TypeDefinition si name@(FQN _ localName) params type' ->
      return (env `extend` (localName, Type si name params type'), def')

    Core.ForeignDefinition _ name _ ->
      error ("unexpected foreign definition: " ++ asString name)

-- | Infer the package, returning a package with typed definitions along with
-- the extended typing environment.
inferPackage :: Untyped.Package [Core.ImportedPackage]
             -> Either TypeError Core.Package
inferPackage (Untyped.Package (Untyped.PackageDeclaration psi name) imports defs) = do
  let env = fromPackage universe `merge` fromPackages imports
  inferred <- snd <$> foldM iter (env, []) defs
  return (Core.Package (Core.PackageDeclaration psi name) imports inferred)
  where
  iter (e, inferred) def = do
      (e', def') <- inferDefinition e def
      return (e', inferred ++ [def'])

-- | Swaps all type variables names for generated ones based on 'letters' to
-- get a nice sequence.
normalize :: (Scheme, Core.Expr Type) -> (Scheme, Core.Expr Type)
normalize (Forall si _ exprType, te) =
  (Forall si newBindings (apply subst exprType), apply subst te)
  where
    -- Pairs of existing type variables in the type and new type variables
    -- values to substitute with, a sequence based on 'letters'.
    substPairs = zip (Set.toList $ ftv exprType) (map TV letters)
    -- The substitution based on the pairs.
    subst = Subst (Map.fromList (map wrapTvar substPairs))
    wrapTvar (tv1, tv2) = (tv1, TVar (Metadata Missing) tv2)
    -- The new set of type variables bindings for the canonical expression.
    newBindings = map (TVarBinding (Metadata Missing) . snd) substPairs
