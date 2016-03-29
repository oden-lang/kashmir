-- | A representation of a subset of the Go Programming Language, based on
-- https://golang.org/ref/spec. Constructs not needed in Oden are excluded.
module Oden.Go.AST where

import Oden.Go.Identifier
import Oden.Go.Type

data StringLiteral = RawStringLiteral String
                   | InterpretedStringLiteral String
                   deriving (Show, Eq)

data BasicLiteral = IntLiteral Integer
                  | FloatLiteral Float
                  | RuneLiteral Char
                  | StringLiteral StringLiteral
                  deriving (Show, Eq)

data LiteralKey = LiteralKeyName Identifier
                | LiteralKeyExpression Expression
                | LiteralKeyValue LiteralValueElements
                deriving (Show, Eq)

data LiteralElement = UnkeyedElement Expression
                    | KeyedElement LiteralKey Expression
                    deriving (Show, Eq)

data LiteralValueElements = LiteralValueElements [LiteralElement]
                  deriving (Show, Eq)

data FunctionParameter = FunctionParameter Identifier Type
                       | VariadicFunctionParameter Identifier Type
                       deriving (Show, Eq)

data FunctionSignature = FunctionSignature [FunctionParameter] [Type]
                       deriving (Show, Eq)

data Literal = BasicLiteral BasicLiteral
             | CompositeLiteral Type LiteralValueElements
             | FunctionLiteral FunctionSignature Block
             deriving (Show, Eq)

data Operand = Literal Literal
             | OperandName Identifier
             | QualifiedOperandName Identifier Identifier
             | GroupedExpression Expression
             deriving (Show, Eq)

data SliceExpression = ClosedSlice Expression Expression
                     | LowerBoundSlice Expression
                     | UpperBoundSlice Expression
                     deriving (Show, Eq)

data Argument = Argument Expression
              | VariadicSliceArgument Expression
              deriving (Show, Eq)

data PrimaryExpression = Operand Operand
                       | Conversion Type Expression
                       | Selector PrimaryExpression Identifier
                       | Index PrimaryExpression Expression
                       | Slice PrimaryExpression SliceExpression
                       | Application PrimaryExpression [Argument]
                       deriving (Show, Eq)

data UnaryOperator = UnaryPlus
                   | UnaryNegation
                   | Not
                   | BitwiseComplement
                   | AddressOf
                   deriving (Show, Eq)

data BinaryOperator = Or
                    | And
                    | EQ
                    | NEQ
                    | LT
                    | LTE
                    | GT
                    | GTE
                    | Sum
                    | Difference
                    | Product
                    | Quotient
                    | Remainder
                    | BitwiseOR
                    | BitwiseXOR
                    | BitwiseAND
                    | BitwiseClear
                    | LeftShift
                    | RightShift
                    deriving (Show, Eq)

data Expression = Expression PrimaryExpression
                | UnaryOp UnaryOperator PrimaryExpression
                | BinaryOp BinaryOperator Expression Expression
                deriving (Show, Eq)

data AssignOp = Assign | AssignSum | AssignProduct deriving (Show, Eq)

data SimpleStmt = ExpressionStmt Expression
                | Assignment [Expression] AssignOp [Expression]
                | SendStmt Expression Expression
                deriving (Show, Eq)

data ElseBranch = ElseBlock Block
                | ElseIf IfStmt
                deriving (Show, Eq)

data IfStmt = If Expression Block
            | IfElse Expression Block ElseBranch
            deriving (Show, Eq)

data Stmt = DeclarationStmt Declaration
          | IfStmt IfStmt
          | ReturnStmt [Expression]
          | BlockStmt Block
          | GoStmt Expression
          | SimpleStmt SimpleStmt
          deriving (Show, Eq)

data Block = Block [Stmt] deriving (Show, Eq)

data VarDeclaration = VarDeclZeroValue Identifier Type
                    | VarDeclInitializer Identifier Type Expression
                    deriving (Show, Eq)

data Declaration = TypeDecl Identifier Type
                 | ConstDecl Identifier Type Expression
                 | VarDecl VarDeclaration
                 deriving (Show, Eq)

data TopLevelDeclaration = Decl Declaration
                         | FunctionDecl Identifier FunctionSignature Block
                         deriving (Show, Eq)

data PackageClause = PackageClause Identifier
                   deriving (Show, Eq)

data ImportDecl = ImportDecl Identifier StringLiteral
                deriving (Show, Eq)

data SourceFile = SourceFile PackageClause [ImportDecl] [TopLevelDeclaration]
                deriving (Show, Eq)
