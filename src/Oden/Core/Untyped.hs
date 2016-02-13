module Oden.Core.Untyped where

import           Oden.Identifier
import           Oden.Core.Operator
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data Binding = Binding SourceInfo Name
             deriving (Show, Eq, Ord)

data Expr = Symbol SourceInfo Identifier
          | Op1 SourceInfo UnaryOperator Expr
          | Op SourceInfo BinaryOperator Expr Expr
          | Application SourceInfo Expr [Expr]
          | Fn SourceInfo Binding Expr
          | NoArgFn SourceInfo Expr
          | Let SourceInfo Binding Expr Expr
          | Literal SourceInfo Literal
          | Tuple SourceInfo Expr Expr [Expr]
          | If SourceInfo Expr Expr Expr
          | Slice SourceInfo [Expr]
          | Block SourceInfo [Expr]
          deriving (Show, Eq, Ord)

instance HasSourceInfo Expr where
  getSourceInfo (Symbol si _)                   = si
  getSourceInfo (Op1 si _ _)                    = si
  getSourceInfo (Op si _ _ _)                   = si
  getSourceInfo (Application si _ _)            = si
  getSourceInfo (Fn si _ _)                     = si
  getSourceInfo (NoArgFn si _)                  = si
  getSourceInfo (Let si _ _ _)                  = si
  getSourceInfo (Literal si _)                  = si
  getSourceInfo (If si _ _ _)                   = si
  getSourceInfo (Slice si _)                    = si
  getSourceInfo (Tuple si _ _ _)                = si
  getSourceInfo (Block si _)                    = si

  setSourceInfo si (Symbol _ i)                   = Symbol si i
  setSourceInfo si (Op1 _ p r)                    = Op1 si p r
  setSourceInfo si (Op _ p l r)                   = Op si p l r
  setSourceInfo si (Application _ f a)            = Application si f a
  setSourceInfo si (Fn _ n b)                     = Fn si n b
  setSourceInfo si (NoArgFn _ b)                  = NoArgFn si b
  setSourceInfo si (Let _ n v b)                  = Let si n v b
  setSourceInfo si (Literal _ l)                  = Literal si l
  setSourceInfo si (If _ c t e)                  = If si c t e
  setSourceInfo si (Slice _ e)                    = Slice si e
  setSourceInfo si (Tuple _ f s r)                = Tuple si f s r
  setSourceInfo si (Block _ e)                    = Block si e

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

data Definition = Definition SourceInfo Name (Maybe Scheme) Expr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data Import = Import SourceInfo PackageName
            deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [Import] [Definition]
             deriving (Show, Eq, Ord)
