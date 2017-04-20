{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
module Oden.Compiler.Validation.Typed where

import           Oden.Core.Typed
import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Traversal

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName        (PackageName(..), QualifiedName (..), nameInUniverse)
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Oden.Compiler.LiteralEval

import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.Except

import qualified Data.Set                  as Set
import           Data.List

data ValidationError
  = ValueDiscarded TypedExpr
  | DivisionByZero TypedExpr
  | NegativeSliceIndex TypedExpr
  | InvalidSubslice SourceInfo TypedRange
  | UnusedImport SourceInfo PackageName Identifier
  | MainPkgDoesNotHaveMainFn SourceInfo
  | MainPkgDoesNotHaveValidMainFn SourceInfo
  deriving (Show, Eq, Ord)

data ValidationWarning = ValidationWarning -- There's no warnings defined yet.
                       deriving (Show, Eq, Ord)

data ValidationState = ValidationState { packageReferences :: Set.Set Identifier }

initState :: ValidationState
initState = ValidationState {
  packageReferences = Set.empty
}

type Validate = RWST
                (Set.Set Identifier)
                [ValidationWarning]
                ValidationState
                (Except ValidationError)

addPackageReference :: Identifier -> Validate ()
addPackageReference name = do
  refs <- gets packageReferences
  modify (\s -> s { packageReferences = Set.insert name refs })

validateSliceIndex :: TypedExpr -> Validate ()
validateSliceIndex e = do
  validateExpr e
  case evaluate e of
    Just (Int n) | n < 0 -> throwError $ NegativeSliceIndex e
    _ -> return ()


validateRange :: TypedRange -> Validate ()
validateRange r@(Range e1 e2) = do
  validateSliceIndex e1
  validateSliceIndex e2
  case (evaluate e1, evaluate e2) of
    (Just (Int v1), Just (Int v2)) |
      v1 > v2 -> throwError $ InvalidSubslice (getSourceInfo e1) r
    _ -> return ()
validateRange (RangeTo e) =
  validateSliceIndex e
validateRange (RangeFrom e) =
  validateSliceIndex e

validateExpr :: TypedExpr -> Validate ()
validateExpr = void . traverseExpr identityTraversal { onExpr = onExpr'
                                                     , onMemberAccess = onMemberAccess'
                                                     }
  where
  onExpr' expr = case expr of
    Subscript _ _ i _ -> do
      validateSliceIndex i
      return $ Just expr
    Subslice _ _ r _ -> do
      validateRange r
      return $ Just expr
    Application _ (Application _ (MethodReference _ ref _) _ _) (Literal _ (Int 0) _) _
      | isDivideRef ref -> throwError $ DivisionByZero expr
      | otherwise -> return Nothing
    Block _ exprs _ -> do
      mapM_ warnOnDiscarded (init exprs)
      return $ Just expr
      where
      warnOnDiscarded :: TypedExpr -> Validate ()
      warnOnDiscarded e = case typeOf e of
        TCon _ (FQN (NativePackageName []) (Identifier "unit")) -> return ()
        _                                   -> throwError (ValueDiscarded e)
    _ -> return Nothing

  onMemberAccess' p@(PackageMemberAccess alias _) = do
    addPackageReference alias
    return p
  onMemberAccess' a = return a

  divisionName = nameInUniverse "Num"
  divideName = Identifier "Divide"
  isDivideRef =
    \case
      Unresolved protocol method _ -> protocol == divisionName && method == divideName
      Resolved protocol method _ -> protocol == divisionName && method == divideName


validateMainPkg :: PackageDeclaration -> [TypedDefinition] -> Validate ()
validateMainPkg (PackageDeclaration si _) defs = case find isMainFn defs of
    Just (Definition _ _ (_, NoArgFn _ _ (TNoArgFn _ (TCon _ (FQN (NativePackageName []) (Identifier "unit")))))) -> return ()
    Just def -> throwError . MainPkgDoesNotHaveValidMainFn . getSourceInfo $ def
    Nothing -> throwError . MainPkgDoesNotHaveMainFn . unwrap $ si
    where
    isMainFn =
      \case
        Definition _ (FQN _ (Identifier "main")) _ -> True
        _ -> False


repeated :: [(Identifier, Type)] -> [(Identifier, Type)]
repeated fields = snd (foldl check (Set.empty, []) fields)
  where
  check (names, fields') f@(n, _)
    | n `Set.member` names = (names, fields'++ [f])
    | otherwise            = (Set.insert n names, fields')


validatePackage :: TypedPackage -> Validate ()
validatePackage (TypedPackage pkgDecl imports definitions) = do
  -- Validates all definitions and expressions recursively. Also collects usage
  -- of imported packages.
  validateDefs definitions
  when isMainPkg $ validateMainPkg pkgDecl definitions
  -- When package usage is collected we can validate the imports.
  mapM_ validateImport imports
  where
  validateImport (ImportedPackage ref alias (TypedPackage (PackageDeclaration _ pkg) _ _)) = do
    refs <- gets packageReferences
    unless (alias `Set.member` refs) $
      throwError (UnusedImport (getSourceInfo ref) pkg alias)
  validateDefs =
    \case
      Definition _ _ (_, expr) : defs -> do
        validateExpr expr
        validateDefs defs
      Implementation _ (ProtocolImplementation _ _ _ methods) : defs -> do
        mapM_ validateMethod methods
        validateDefs defs
        where
        validateMethod (MethodImplementation _ _ expr) =
          validateExpr expr
      _ : defs -> validateDefs defs
      [] -> return ()
  isMainPkg = packageDeclarationName pkgDecl == NativePackageName ["main"]

validate :: TypedPackage -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (snd <$> evalRWST (validatePackage pkg) Set.empty initState)
