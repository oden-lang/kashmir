module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeTopLevel,
 explodePackage
) where

import qualified Oden.Core.Untyped     as Untyped
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (QualifiedName(..))
import           Oden.Syntax
import           Oden.SourceInfo
import           Oden.Type.Signature

import           Control.Monad
import           Control.Monad.Writer
import qualified Data.Map              as Map

data ExplodeError = TypeSignatureWithoutDefinition SourceInfo Identifier (TypeSignature SourceInfo)
                  | TypeSignatureRedefinition SourceInfo Identifier (Maybe (TypeSignature SourceInfo))
                  | InvalidMemberAccessExpression SourceInfo Untyped.Expr Untyped.Expr
                  deriving (Show, Eq)

explodeNameBinding :: NameBinding -> Untyped.NameBinding
explodeNameBinding (NameBinding si name) = Untyped.NameBinding (Metadata si) name

toEither :: (Eq v, Show v, Show e) => Writer [e] v -> Either [e] v
toEither w = case es of
               [] -> Right a
               _  -> Left es
  where (a, es) = runWriter w

explodeExpr :: Expr -> Either [ExplodeError] Untyped.Expr
explodeExpr = toEither . explodeExpr'

explodePackage :: Package -> Either [ExplodeError] (Untyped.Package [Untyped.ImportReference])
explodePackage = toEither . explodePackage'

explodeTopLevel :: PackageName -> [TopLevel] -> Either [ExplodeError] ([Untyped.ImportReference], [Untyped.Definition])
explodeTopLevel = (.) toEither . explodeTopLevel'

explodeExpr' :: Expr -> Writer [ExplodeError] Untyped.Expr
explodeExpr' (Subscript si es [Singular e]) =
  Untyped.Subscript (Metadata si) <$> explodeExpr' es <*> explodeExpr' e
explodeExpr' (Subscript si es [Range e1 e2]) =
  Untyped.Subslice (Metadata si) <$> explodeExpr' es <*> (Untyped.Range <$> explodeExpr' e1 <*> explodeExpr' e2)
explodeExpr' (Subscript si es [RangeTo e]) =
  Untyped.Subslice (Metadata si) <$> explodeExpr' es <*> (Untyped.RangeTo <$> explodeExpr' e)
explodeExpr' (Subscript si es [RangeFrom e]) =
  Untyped.Subslice (Metadata si) <$> explodeExpr' es <*> (Untyped.RangeFrom <$> explodeExpr' e)
explodeExpr' (Subscript si es (i:ir)) =
  explodeExpr' (Subscript si (Subscript si es [i]) ir)

explodeExpr' (UnaryOp si o e) =
  Untyped.UnaryOp (Metadata si) o <$> explodeExpr' e
explodeExpr' (BinaryOp si o e1 e2) =
  Untyped.BinaryOp (Metadata si) o <$> explodeExpr' e1 <*> explodeExpr' e2
explodeExpr' (Symbol si i) =
  return $ Untyped.Symbol (Metadata si) i
explodeExpr' (Literal si (Bool b)) =
  return $ Untyped.Literal (Metadata si) (Untyped.Bool b)
explodeExpr' (Literal si (Int i)) =
  return $ Untyped.Literal (Metadata si) (Untyped.Int i)
explodeExpr' (Literal si (String s)) =
  return $ Untyped.Literal (Metadata si) (Untyped.String s)
explodeExpr' (Literal si Unit) =
  return $ Untyped.Literal (Metadata si) Untyped.Unit
explodeExpr' (Tuple si f s r) =
  Untyped.Tuple (Metadata si) <$> explodeExpr' f <*> explodeExpr' s <*> mapM explodeExpr' r
explodeExpr' (If si c t f) =
  Untyped.If (Metadata si) <$> explodeExpr' c <*> explodeExpr' t <*> explodeExpr' f
explodeExpr' (Application si f ps) =
  Untyped.Application (Metadata si) <$> explodeExpr' f <*> mapM explodeExpr' ps
explodeExpr' (Fn si [] b) =
  Untyped.NoArgFn (Metadata si) <$> explodeExpr' b
explodeExpr' (Fn si [arg] b) =
  Untyped.Fn (Metadata si) (explodeNameBinding arg) <$> explodeExpr' b
explodeExpr' (Fn si (arg:args) b) =
  Untyped.Fn (Metadata si) (explodeNameBinding arg) <$> explodeExpr' (Fn si args b)
explodeExpr' (StructInitializer si ts exprs) =
  Untyped.StructInitializer (Metadata si) ts <$> mapM explodeExpr' exprs
explodeExpr' (MemberAccess si expr (Symbol _ name)) =
  Untyped.MemberAccess (Metadata si) <$> explodeExpr' expr <*> return name
explodeExpr' (MemberAccess si expr nonName) = do
  expr' <- explodeExpr' expr
  nonName' <- explodeExpr' nonName
  tell [InvalidMemberAccessExpression si expr' nonName']
  return expr'

-- invalid, but can be handled anyway
explodeExpr' (Subscript _ a []) = explodeExpr' a
explodeExpr' (Let _ [] b) = explodeExpr' b
explodeExpr' (Let _ [LetPair si n e] b) =
  Untyped.Let (Metadata si) (explodeNameBinding n) <$> explodeExpr' e <*> explodeExpr' b
explodeExpr' (Let si (LetPair _ n e:bs) b) =
  Untyped.Let (Metadata si) (explodeNameBinding n) <$> explodeExpr' e <*> explodeExpr' (Let si bs b)
explodeExpr' (Slice si es) =
  Untyped.Slice (Metadata si) <$> mapM explodeExpr' es
explodeExpr' (Block si es) =
  Untyped.Block (Metadata si) <$> mapM explodeExpr' es

-- temporary metadata for top level definitions, used for keeping track
-- of duplications and detecting missing terms for type signatures
data TempTopLevel = TempTop {
    tempType :: (SourceInfo, Maybe (TypeSignature SourceInfo)),
    -- whether this signature has a corresponding definition
    hasValue :: Bool
}

explodeTopLevel' :: PackageName -> [TopLevel] -> Writer [ExplodeError] ([Untyped.ImportReference], [Untyped.Definition])
explodeTopLevel' pkg top = do
  (is, scs, defs) <- foldM iter ([], Map.empty, []) top
  case filter (not . hasValue . snd) (Map.assocs scs) of
    [] -> return ()
    us -> mapM_ (tell . (:[]) . typeSigNoValErr) us
  return (is, defs)
  where
  iter (is, ts, defs) (FnDefinition si name args body) = do
    def <- Untyped.Definition (Metadata si) name (Map.lookup name ts >>= snd . tempType) <$> explodeExpr' (Fn si args body)
    return (is, assignValue name si ts, defs ++ [def])
  iter (is, ts, defs) (ValueDefinition si name expr) = do
    def <- Untyped.Definition (Metadata si) name (Map.lookup name ts >>= snd . tempType) <$> explodeExpr' expr
    return (is, assignValue name si ts, defs ++ [def])
  iter (is, ts, defs) (TypeSignatureDeclaration tsi name sc) = do
    let (TypeSignature sc' _ _) = sc
    case Map.lookup name ts of
      Just existing -> tell [typeSigReDefErr (name, existing) sc'] -- type already defined
      Nothing -> return ()
    return (is, newTypeSig name tsi (Just sc) ts, defs)
  iter (is, ts, defs) (ImportDeclaration si name) =
    return (is ++ [Untyped.ImportReference (Metadata si) name], ts, defs)
  iter (is, ts, defs) (TypeDefinition si name typeSig) =
    -- TODO: Add support for type parameters
    let def = Untyped.TypeDefinition (Metadata si) (FQN pkg name) [] typeSig
    in return (is, ts, defs ++ [def])
  newTypeSig name tsi msc
    = Map.insertWith (\_ old -> old) name (TempTop (tsi, msc) False)
  assignValue name si
    -- if there's type signature, keep its location
    = Map.insertWith (\_ (TempTop (_, sc') _) -> (TempTop (si, sc') True))
                     name (TempTop (si, Nothing) True)
  -- type signature doesn't have an assigned term
  typeSigNoValErr :: (Identifier, TempTopLevel) -> ExplodeError
  typeSigNoValErr (n, TempTop (si, sc) _)
      = case sc of
          Just j_sc -> TypeSignatureWithoutDefinition si n j_sc
          -- if this happens, it's a bug in the compiler, rather than source code
          Nothing  -> error "Panic: type signature definition present without actual signature"
  -- type signature already defined
  typeSigReDefErr :: (Identifier, TempTopLevel) -> SourceInfo -> ExplodeError
  typeSigReDefErr (n, TempTop (_, sc) _) si'
        = TypeSignatureRedefinition si' n sc

explodePackage' :: Package -> Writer [ExplodeError] (Untyped.Package [Untyped.ImportReference])
explodePackage' (Package (PackageDeclaration si name) definitions) = do
  (is, ds) <- explodeTopLevel' name definitions
  return (Untyped.Package (Untyped.PackageDeclaration (Metadata si) name) is ds)
