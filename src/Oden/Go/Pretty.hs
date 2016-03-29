{-# LANGUAGE QuasiQuotes, FlexibleInstances #-}
module Oden.Go.Pretty where

import           Oden.Go.AST        as AST
import           Oden.Go.Identifier
import           Oden.Go.Type       as T

import           Numeric

import           Text.PrettyPrint.Leijen
import           Text.Regex.PCRE.Heavy

indentedInBraces :: Doc -> Doc
indentedInBraces d = vcat [lbrace, indent 4 d, rbrace]

commaSep :: Pretty p => [p] -> Doc
commaSep ps = hcat (punctuate (comma <> space) (map pretty ps))

semiSep :: Pretty p => [p] -> Doc
semiSep ps = hcat (punctuate semi (map pretty ps))

instance Pretty Identifier where
  pretty (Identifier n) = text n

instance Pretty StructField where
  pretty (StructField name type') = pretty name <+> pretty type'

instance Pretty Parameters where
  pretty (Parameters [] _) = parens empty
  pretty (Parameters types True) =
    parens (hcat (punctuate comma (map pretty (init types) ++ [lastParam])))
    where lastParam = pretty (last types) <> text "..."
  pretty (Parameters types False) = parens (commaSep types)

instance Pretty Returns where
  pretty (Returns []) = empty
  pretty (Returns [type']) = pretty type'
  pretty (Returns types) = parens (commaSep types)


instance Pretty InterfaceMethodSpec where
  pretty (Method name parameters (Returns [])) =
    pretty name <> pretty parameters
  pretty (Method name parameters returns) =
    pretty name <> pretty parameters <+> pretty returns
  pretty (Embed name) = pretty name

instance Pretty Type where
  pretty (Basic name _) = pretty name
  pretty (Pointer type') = text "*" <> pretty type'
  pretty (Array size type') = brackets (int size) <> pretty type'
  pretty (T.Slice type') = brackets empty <> pretty type'
  pretty (Signature _ parameters returns) = text "func" <+> pretty parameters <+> pretty returns
  pretty (Struct []) = text "struct{}"
  pretty (Struct fields) = text "struct" <+> indentedInBraces (vcat (map pretty fields))
  pretty (Named _ name _) = pretty name
  pretty (Interface []) = text "interface{}"
  pretty (Interface methods) = text "interface" <+> indentedInBraces (vcat (map pretty methods))
  pretty (Unsupported s) = error ("Cannot pretty print unsupported: " ++ s)

showGoString :: Show a => a -> String
showGoString s = gsub ([re|(\\)(\d+)|]) toHex (show s)
  where
  toHex (_:n:_) = "\\U" ++ pad 8 (showHex (read n :: Int) "")
  toHex (m:_) = m
  toHex [] = ""
  pad :: Int -> String -> String
  pad n s'
      | length s' < n  = replicate (n - length s') '0' ++ s'
      | otherwise      = s'

instance Pretty StringLiteral where
  pretty (RawStringLiteral s) =
    text "`" <> text s <> text "`"
  pretty (InterpretedStringLiteral s) =
    text (showGoString s)

instance Pretty BasicLiteral where
  pretty (IntLiteral n) = integer n
  pretty (FloatLiteral n) = float n
  pretty (RuneLiteral c) = squotes (char c)
  pretty (StringLiteral s) = pretty s

instance Pretty LiteralKey where
  pretty (LiteralKeyName name) = pretty name
  pretty (LiteralKeyExpression expr) = pretty expr
  pretty (LiteralKeyValue elements) = pretty elements

instance Pretty LiteralElement where
  pretty (UnkeyedElement expr) = pretty expr
  pretty (KeyedElement key expr) = pretty key <> colon <+> pretty expr

instance Pretty LiteralValueElements where
  pretty (LiteralValueElements elements) = braces (commaSep elements)

instance Pretty FunctionParameter where
  pretty (FunctionParameter name type') = pretty name <+> pretty type'
  pretty (VariadicFunctionParameter name type') = pretty name <+> text "..." <> pretty type'

instance Pretty FunctionSignature where
  pretty (FunctionSignature params returns) =
    case returns of
      []        -> parens (commaSep params)
      [return'] -> parens (commaSep params) <+> pretty return'
      _         -> parens (commaSep params) <+> parens (commaSep returns)

instance Pretty Literal where
  pretty (BasicLiteral basic) = pretty basic
  pretty (CompositeLiteral type' elements) = pretty type' <> pretty elements
  pretty (FunctionLiteral signature block) = text "func" <+> pretty signature <+> pretty block

instance Pretty Operand where
  pretty (Literal literal) = pretty literal
  pretty (OperandName name) = pretty name
  pretty (QualifiedOperandName pkg literal) = pretty pkg <> dot <> pretty literal
  pretty (GroupedExpression expr) = parens (pretty expr)

instance Pretty SliceExpression where
  pretty (ClosedSlice lowerExpr upperExpr) = pretty lowerExpr <> colon <> pretty upperExpr
  pretty (LowerBoundSlice lowerExpr) = pretty lowerExpr <> colon
  pretty (UpperBoundSlice upperExpr) = colon <> pretty upperExpr

instance Pretty Argument where
  pretty (Argument expr) = pretty expr
  pretty (VariadicSliceArgument expr) = pretty expr <> text "..."

instance Pretty PrimaryExpression where
  pretty (Operand operand) = pretty operand
  pretty (Conversion type' operand) = pretty type' <> parens (pretty operand)
  pretty (Selector expr field) = pretty expr <> dot <> pretty field
  pretty (Index expr indexExpr) = pretty expr <> brackets (pretty indexExpr)
  pretty (AST.Slice expr sliceExpr) = pretty expr <> brackets (pretty sliceExpr)
  pretty (Application func args) = pretty func <> parens (commaSep args)

instance Pretty UnaryOperator where
  pretty op = case op of
    UnaryPlus         -> text "+"
    UnaryNegation     -> text "-"
    Not               -> text "!"
    BitwiseComplement -> text "^"
    AddressOf         -> text "&"

instance Pretty BinaryOperator where
  pretty op = case op of
    Or           -> text "||"
    And          -> text "&&"
    AST.EQ       -> text "=="
    NEQ          -> text "!="
    AST.LT       -> text "<"
    LTE          -> text "<="
    AST.GT       -> text ">"
    GTE          -> text ">="
    Sum          -> text "+"
    Difference   -> text "-"
    Product      -> text "*"
    Quotient     -> text "/"
    Remainder    -> text "%"
    BitwiseOR    -> text "|"
    BitwiseXOR   -> text "^"
    BitwiseAND   -> text "&"
    BitwiseClear -> text "^&"
    LeftShift    -> text "<<"
    RightShift   -> text ">>"

instance Pretty Expression where
  pretty (Expression primary) = pretty primary
  pretty (UnaryOp operator expr) = pretty operator <> pretty expr
  pretty (BinaryOp operator lhs rhs) = pretty lhs <+> pretty operator <+> pretty rhs

instance Pretty AssignOp where
  pretty op = case op of
    Assign        -> text "="
    AssignSum     -> text "=+"
    AssignProduct -> text "=*"

instance Pretty SimpleStmt where
  pretty (ExpressionStmt expr) = pretty expr
  pretty (Assignment lhs op rhs) = pretty lhs <+> pretty op <+> pretty rhs
  pretty (SendStmt channel expr) = pretty channel <+> text "<-" <+> pretty expr

instance Pretty ElseBranch where
  pretty (ElseBlock block) = pretty block
  pretty (ElseIf ifStmt) = pretty ifStmt

instance Pretty IfStmt where
  pretty (If cond block) =
    text "if" <+> pretty cond <+> pretty block
  pretty (IfElse cond block elseBranch) =
    text "if" <+> pretty cond <+> pretty block <+> text "else" <+> pretty elseBranch

instance Pretty Stmt where
  pretty (DeclarationStmt decl) = pretty decl
  pretty (IfStmt ifStmt) = pretty ifStmt
  pretty (ReturnStmt []) = text "return"
  pretty (ReturnStmt exprs) = text "return" <+> commaSep exprs
  pretty (BlockStmt block) = pretty block
  pretty (SimpleStmt stmt) = pretty stmt
  pretty _ = text "// todo"

instance Pretty Block where
  pretty (Block []) = braces empty
  pretty (Block stmts) = indentedInBraces (vcat $ map pretty stmts)

instance Pretty VarDeclaration where
  pretty (VarDeclZeroValue name type') =
    text "var" <+> pretty name <+> pretty type'
  pretty (VarDeclInitializer name type' expr) =
    text "var" <+> pretty name <+> pretty type' <+> equals <+> pretty expr

instance Pretty Declaration where
  pretty (TypeDecl name type') =
    text "type" <+> pretty name <+> pretty type'
  pretty (ConstDecl name type' expr) =
    text "const" <+> pretty name <+> pretty type' <+> equals <+> pretty expr
  pretty (VarDecl decl) = pretty decl

instance Pretty ImportDecl where
  pretty (ImportDecl name path) =
    text "import" <+> pretty name <+> pretty path

instance Pretty TopLevelDeclaration where
  pretty (Decl decl) = pretty decl
  pretty (FunctionDecl name signature block) =
    text "func" <+> pretty name <> pretty signature <+> pretty block

instance Pretty PackageClause where
  pretty (PackageClause name) =
    text "package" <+> pretty name

instance Pretty SourceFile where
  pretty (SourceFile pkgClause imports declarations) =
    vcat (punctuate line parts) <> line
    where parts = pretty pkgClause : map pretty imports ++ map pretty declarations
