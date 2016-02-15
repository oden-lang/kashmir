module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeTopLevel,
 explodePackage
) where

import qualified Oden.Core.Untyped     as Untyped
import           Oden.Identifier
import           Oden.Syntax
import           Oden.SourceInfo
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import qualified Data.Map              as Map
import qualified Data.Set              as Set

data ExplodeError = TypeSignatureWithoutDefinition SourceInfo Name Scheme
                  deriving (Show, Eq)

explodeBinding :: Binding -> Untyped.Binding
explodeBinding (Binding si name) = Untyped.Binding si name

explodeExpr :: Expr -> Untyped.Expr
explodeExpr (Subscript si a [i]) =
  Untyped.Subscript si (explodeExpr a) (explodeExpr i)
explodeExpr (Subscript si a (i:ir)) =
  explodeExpr (Subscript si (Subscript si a [i]) ir)
explodeExpr (UnaryOp si o e) =
  Untyped.UnaryOp si o (explodeExpr e)
explodeExpr (BinaryOp si o e1 e2) =
  Untyped.BinaryOp si o (explodeExpr e1) (explodeExpr e2)
explodeExpr (Symbol si i) =
  Untyped.Symbol si i
explodeExpr (Literal si (Bool b)) =
  Untyped.Literal si (Untyped.Bool b)
explodeExpr (Literal si (Int i)) =
  Untyped.Literal si (Untyped.Int i)
explodeExpr (Literal si (String s)) =
  Untyped.Literal si (Untyped.String s)
explodeExpr (Literal si Unit) =
  Untyped.Literal si Untyped.Unit
explodeExpr (Tuple si f s r) =
  Untyped.Tuple si (explodeExpr f) (explodeExpr s) (map explodeExpr r)
explodeExpr (If si c t f) =
  Untyped.If si (explodeExpr c) (explodeExpr t) (explodeExpr f)
explodeExpr (Application si f ps) =
  Untyped.Application si (explodeExpr f) (map explodeExpr ps)
explodeExpr (Fn si [] b) =
  Untyped.NoArgFn si (explodeExpr b)
explodeExpr (Fn si [arg] b) =
  Untyped.Fn si (explodeBinding arg) (explodeExpr b)
explodeExpr (Fn si (arg:args) b) =
  Untyped.Fn si (explodeBinding arg) (explodeExpr (Fn si args b))

-- invalid, but can be handled anyway
explodeExpr (Subscript _ a []) = explodeExpr a
explodeExpr (Let _ [] b) = explodeExpr b
explodeExpr (Let _ [LetPair si n e] b) =
  Untyped.Let si (explodeBinding n) (explodeExpr e) (explodeExpr b)
explodeExpr (Let si (LetPair _ n e:bs) b) =
  Untyped.Let si (explodeBinding n) (explodeExpr e) (explodeExpr (Let si bs b)) 
explodeExpr (Slice si es) =
  Untyped.Slice si (map explodeExpr es)
explodeExpr (Block si es) =
  Untyped.Block si (map explodeExpr es)

explodeType :: TypeExpr -> Type
explodeType (TEAny si) = TAny si
explodeType (TEUnit si) = TUnit si
explodeType (TEBasic si TEInt) = TBasic si TInt
explodeType (TEBasic si TEBool) = TBasic si TBool
explodeType (TEBasic si TEString) = TBasic si TString
explodeType (TEVar si s) = TVar si (TV s)
explodeType (TECon si s) = TCon si s
explodeType (TEFn _ d []) = explodeType d
explodeType (TEFn si d (r:rs)) =
  TFn si (explodeType d) (explodeType (TEFn si r rs))
explodeType (TENoArgFn si r) = TNoArgFn si (explodeType r)
explodeType (TETuple si f s r) =
  TTuple si (explodeType f) (explodeType s) (map explodeType r)
explodeType (TESlice si t) = TSlice si (explodeType t)

explodeTVarBinding :: TVarBindingExpr -> TVarBinding
explodeTVarBinding (TVarBindingExpr si n) = TVarBinding si (TV n)

explodeScheme :: SchemeExpr -> Scheme
explodeScheme (Implicit si t) =
  let t' = explodeType t
      bindings = map (TVarBinding Missing) (Set.toList (ftv t'))
  in Forall si bindings t'
explodeScheme (Explicit si vars t) =
  let t' = explodeType t
  in Forall si (map explodeTVarBinding vars) t'

explodeTopLevel :: [TopLevel] -> Either [ExplodeError] ([Untyped.Import], [Untyped.Definition])
explodeTopLevel top =
  let (is, scs, defs) = foldl iter ([], Map.empty, []) top
  in case Map.assocs scs of
    [] -> Right (is, defs)
    as -> Left (map toSignatureError as)
  where iter (is, ts, defs) (FnDefinition si name args body) =
          let def = Untyped.Definition si name (snd <$> Map.lookup name ts) (explodeExpr (Fn si args body))
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (ValueDefinition si name expr) =
          let def = Untyped.Definition si name (snd <$> Map.lookup name ts) (explodeExpr expr)
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (TypeSignature tsi name sc) =
          (is, Map.insert name (tsi, explodeScheme sc) ts, defs)
        iter (is, ts, defs) (ImportDeclaration si name) =
          (is ++ [Untyped.Import si name], ts, defs)
        toSignatureError :: (Name, (SourceInfo, Scheme)) -> ExplodeError
        toSignatureError (n, (si, sc)) = TypeSignatureWithoutDefinition si n sc

explodePackage :: Package -> Either [ExplodeError] Untyped.Package
explodePackage (Package (PackageDeclaration si name) definitions) = do
  (is, ds) <- explodeTopLevel definitions
  return (Untyped.Package (Untyped.PackageDeclaration si name) is ds)
