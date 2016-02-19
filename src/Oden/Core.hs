{-# LANGUAGE TypeSynonymInstances #-}
module Oden.Core where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.SourceInfo
import qualified Oden.Type.Polymorphic as Poly

data Binding = Binding SourceInfo Name
              deriving (Show, Eq, Ord)

data Expr t = Symbol SourceInfo Identifier t
            | Subscript SourceInfo (Expr t) (Expr t) t
            | Subslice SourceInfo (Expr t) (Expr t) (Expr t) t
            | UnaryOp SourceInfo UnaryOperator (Expr t) t
            | BinaryOp SourceInfo BinaryOperator (Expr t) (Expr t) t
            | Application SourceInfo (Expr t) (Expr t) t
            | NoArgApplication SourceInfo (Expr t) t
            | UncurriedFnApplication SourceInfo (Expr t) [Expr t] t
            | Fn SourceInfo Binding (Expr t) t
            | NoArgFn SourceInfo (Expr t) t
            | Let SourceInfo Binding (Expr t) (Expr t) t
            | Literal SourceInfo Literal t
            | Tuple SourceInfo (Expr t) (Expr t) [Expr t] t
            | If SourceInfo (Expr t) (Expr t) (Expr t) t
            | Slice SourceInfo [Expr t] t
            | Block SourceInfo [Expr t] t
            deriving (Show, Eq, Ord)

instance HasSourceInfo (Expr t) where
  getSourceInfo (Symbol si _ _)                   = si
  getSourceInfo (Subscript si _ _ _)              = si
  getSourceInfo (Subslice si _ _ _ _)             = si
  getSourceInfo (UnaryOp si _ _ _)                = si
  getSourceInfo (BinaryOp si _ _ _ _)             = si
  getSourceInfo (Application si _ _ _)            = si
  getSourceInfo (NoArgApplication si _ _)         = si
  getSourceInfo (UncurriedFnApplication si _ _ _) = si
  getSourceInfo (Fn si _ _ _)                     = si
  getSourceInfo (NoArgFn si _ _)                  = si
  getSourceInfo (Let si _ _ _ _)                  = si
  getSourceInfo (Literal si _ _)                  = si
  getSourceInfo (If si _ _ _ _)                   = si
  getSourceInfo (Slice si _ _)                    = si
  getSourceInfo (Tuple si _ _ _ _)                = si
  getSourceInfo (Block si _ _)                    = si

  setSourceInfo si (Symbol _ i t)                   = Symbol si i t
  setSourceInfo si (Subscript _ s i t)              = Subscript si s i t
  setSourceInfo si (Subslice _ s i1 i2 t)           = Subslice si s i1 i2 t
  setSourceInfo si (UnaryOp _ o r t)                = UnaryOp si o r t
  setSourceInfo si (BinaryOp _ p l r t)             = BinaryOp si p l r t
  setSourceInfo si (Application _ f a t)            = Application si f a t
  setSourceInfo si (NoArgApplication _ f t)         = NoArgApplication si f t
  setSourceInfo si (UncurriedFnApplication _ f a t) = UncurriedFnApplication si f a t
  setSourceInfo si (Fn _ n b t)                     = Fn si n b t
  setSourceInfo si (NoArgFn _ b t)                  = NoArgFn si b t
  setSourceInfo si (Let _ n v b t)                  = Let si n v b t
  setSourceInfo si (Literal _ l t)                  = Literal si l t
  setSourceInfo si (If _ c t e t')                  = If si c t e t'
  setSourceInfo si (Slice _ e t)                    = Slice si e t
  setSourceInfo si (Tuple _ f s r t)                = Tuple si f s r t
  setSourceInfo si (Block _ e t)                    = Block si e t

typeOf :: Expr t -> t
typeOf (Symbol _ _ t) = t
typeOf (Subscript _ _ _ t) = t
typeOf (Subslice _ _ _ _ t) = t
typeOf (UnaryOp _ _ _ t) = t
typeOf (BinaryOp _ _ _ _ t) = t
typeOf (Application _ _ _ t) = t
typeOf (NoArgApplication _ _ t) = t
typeOf (UncurriedFnApplication _ _ _ t) = t
typeOf (Fn _ _ _ t) = t
typeOf (NoArgFn _ _ t) = t
typeOf (Let _ _ _ _ t) = t
typeOf (Literal _ _ t) = t
typeOf (If _ _ _ _ t) = t
typeOf (Tuple _ _ _ _ t) = t
typeOf (Slice _ _ t) = t
typeOf (Block _ _ t) = t

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

type CanonicalExpr = (Poly.Scheme, Expr Poly.Type)

data Definition = Definition SourceInfo Name CanonicalExpr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                        deriving (Show, Eq, Ord)

data Import = Import SourceInfo PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [Import] [Definition]
             deriving (Show, Eq, Ord)
