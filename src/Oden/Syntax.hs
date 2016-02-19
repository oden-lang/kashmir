module Oden.Syntax where

import           Oden.Identifier
import           Oden.Core.Operator
import           Oden.SourceInfo

data Binding = Binding SourceInfo Name
                 deriving (Show, Eq, Ord)

data LetPair = LetPair SourceInfo Binding Expr
             deriving (Show, Eq, Ord)

data Expr = Symbol SourceInfo Identifier
          | Subscript SourceInfo Expr [Subscript]
          | UnaryOp SourceInfo UnaryOperator Expr
          | BinaryOp SourceInfo BinaryOperator Expr Expr
          | Application SourceInfo Expr [Expr]
          | Fn SourceInfo [Binding] Expr
          | Let SourceInfo [LetPair] Expr
          | Literal SourceInfo Literal
          | If SourceInfo Expr Expr Expr
          | Slice SourceInfo [Expr]
          | Tuple SourceInfo Expr Expr [Expr]
          | Block SourceInfo [Expr]
          deriving (Show, Eq, Ord)

data Subscript = Singular Expr
               | Range Expr Expr
               deriving (Show, Eq, Ord)

data Literal = Int Integer
             | Bool Bool
             | String String
             | Unit
             deriving (Show, Eq, Ord)

data BasicTypeExpr = TEInt
                   | TEBool
                   | TEString
                   deriving (Show, Eq, Ord)

data TypeExpr = TEAny SourceInfo
              | TEBasic SourceInfo BasicTypeExpr
              | TEUnit SourceInfo
              | TEVar SourceInfo String
              | TECon SourceInfo String
              | TEFn SourceInfo TypeExpr [TypeExpr]
              | TENoArgFn SourceInfo TypeExpr
              | TETuple SourceInfo TypeExpr TypeExpr [TypeExpr]
              | TESlice SourceInfo TypeExpr
              deriving (Show, Eq, Ord)

data TVarBindingExpr = TVarBindingExpr SourceInfo String
                     deriving (Show, Eq, Ord)

data SchemeExpr = Explicit SourceInfo [TVarBindingExpr] TypeExpr
                | Implicit SourceInfo TypeExpr
                deriving (Show, Eq, Ord)

type PackageName = [Name]

data PackageDeclaration = PackageDeclaration SourceInfo PackageName
                          deriving (Show, Eq, Ord)

data TopLevel = ImportDeclaration SourceInfo PackageName
                | TypeSignature SourceInfo Name SchemeExpr
                | ValueDefinition SourceInfo Name Expr
                | FnDefinition SourceInfo Name [Binding] Expr
                deriving (Show, Eq, Ord)

data Package = Package PackageDeclaration [TopLevel]
             deriving (Show, Eq, Ord)
