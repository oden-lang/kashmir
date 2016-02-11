{-# LANGUAGE OverloadedStrings #-}

module Oden.Parser where

import qualified Data.Text.Lazy        as L
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token     as Tok
import qualified Text.Parsec.Expr      as Ex

import           Oden.Identifier
import           Oden.Lexer            as Lexer
import           Oden.Syntax           as Syntax
import           Oden.SourceInfo
import           Oden.Core.Operator

sign :: Parser (Integer -> Integer)
sign = (char '-' >> return negate)
       <|> (char '+' >> return id)
       <|> return id

integer :: Parser Integer
integer = do
  f <- sign
  n <- Tok.natural lexer
  return (f n)

currentSourceInfo :: Parser SourceInfo
currentSourceInfo = do
  pos <- getPosition
  return (SourceInfo (Position (sourceName pos)
                               (sourceLine pos)
                               (sourceColumn pos)))

symbol :: Parser Expr
symbol = do
  si <- currentSourceInfo
  i <- try qualified <|> unqualified
  return (Symbol si i)
  where
  qualified = Qualified <$> identifier <*> (char '.' *> identifier)
  unqualified = Unqualified <$> identifier

number :: Parser Expr
number = do
  si <- currentSourceInfo
  n <- integer
  return (Literal si (Int (fromIntegral n)))

stringLiteral :: Parser Expr
stringLiteral = do
  si <- currentSourceInfo
  s <- Lexer.string
  return (Literal si (Syntax.String s))

bool :: Parser Expr
bool = do
  si <- currentSourceInfo
  (reserved "true" >> return (Literal si (Bool True)))
   <|> (reserved "false" >> return (Literal si (Bool False)))

block :: Parser Expr
block = do
  si <- currentSourceInfo
  exprs <- braces (whitespace *> (expr `sepBy1` topSeparator) <* whitespace)
  return (Block si exprs)

if' :: Parser Expr
if' = do
  si <- currentSourceInfo
  reserved "if"
  cond <- expr
  spaces
  reserved "then"
  spaces
  t <- expr
  spaces
  reserved "else"
  f <- expr
  return (If si cond t f)

binding :: Parser Binding
binding = do
  si <- currentSourceInfo
  n <- identifier
  return (Binding si n)

fn :: Parser Expr
fn = do
  si <- currentSourceInfo
  reserved "fn"
  args <- many binding
  body <- spaces *> rArrow *> spaces *> expr
  return (Fn si args body)

letPair :: Parser LetPair
letPair = do
  si <- currentSourceInfo
  i <- binding
  reservedOp "="
  e <- expr
  return (LetPair si i e)

let' :: Parser Expr
let' = do
  si <- currentSourceInfo
  reserved "let"
  bindings <- many1 letPair
  reserved "in"
  body <- expr
  return (Let si bindings body)

application :: Parser Expr
application = do
  si <- currentSourceInfo
  f <- symbol <|> parens expr
  args <- parensList expr
  return (Application si f args)

slice :: Parser Expr
slice = do
  si <- currentSourceInfo
  exprs <- char '!' *> brackets (expr `sepBy` comma)
  return (Slice si exprs)

unitExprOrTuple :: Parser Expr
unitExprOrTuple = do
  si <- currentSourceInfo
  exprs <- parensList expr
  case exprs of
    []      -> return (Literal si Unit)
    [e]     -> return e
    (f:s:r) -> return (Tuple si f s r)

term :: Parser Expr
term =
  try fn
  <|> if'
  <|> let'
  <|> stringLiteral
  <|> slice
  <|> bool
  <|> try number
  <|> try application
  <|> symbol
  <|> block
  <|> unitExprOrTuple

tvar :: Parser String
tvar = char '#' *> identifier

basic :: Parser BasicTypeExpr
basic = (return TEInt <* reserved "int")
    <|> (return TEString <* reserved "string")
    <|> (return TEBool <* reserved "bool")

type' :: Parser TypeExpr
type' = do
  si <- currentSourceInfo
  ts <- simple `sepBy1` rArrow
  case ts of
    [t] -> return t
    (d:r) -> return (TEFn si d r)
    [] -> fail ""
  where
  simple :: Parser TypeExpr
  simple = slice'
        <|> noArgFn
        <|> any'
        <|> basic'
        <|> con
        <|> var
        <|> unitExprOrTupleType
  var = TEVar <$> currentSourceInfo <*> tvar
  any' = do
    si <- currentSourceInfo
    reserved "any"
    return (TEAny si)
  basic' = TEBasic <$> currentSourceInfo <*> basic
  con = TECon <$> currentSourceInfo <*> identifier
  noArgFn = TENoArgFn <$> currentSourceInfo <*> (rArrow *> type')
  unitExprOrTupleType = do
    si <- currentSourceInfo
    ts <- parensList type'
    case ts of
      [] -> return (TEUnit si)
      [t] -> return t
      (f:s:r) -> return (TETuple si f s r)
  slice' = TESlice <$> currentSourceInfo <*> (char '!' *> brackets type')

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

infixOp :: String -> BinaryOperator -> Ex.Assoc -> Op Expr
infixOp x o = Ex.Infix $ do
  si <- currentSourceInfo
  reservedOp x
  return (Op si o)

prefixOp :: String -> UnaryOperator -> Op Expr
prefixOp x o = Ex.Prefix $ do
  si <- currentSourceInfo
  reservedOp x
  return (Op1 si o)

table :: Operators Expr
table = [
    [
      prefixOp "-" Negate,
      prefixOp "!" Not
    ],
    [
      infixOp "*" Multiply Ex.AssocLeft,
      infixOp "/" Divide Ex.AssocLeft
    ],
    [
      infixOp "+" Add Ex.AssocLeft,
      infixOp "-" Subtract Ex.AssocLeft,

      infixOp "++" Concat Ex.AssocLeft
    ],
    [
      infixOp "<" LessThan Ex.AssocLeft,
      infixOp ">" GreaterThan Ex.AssocLeft,
      infixOp "<=" LessThanEqual Ex.AssocLeft,
      infixOp ">=" GreaterThanEqual Ex.AssocLeft,
      infixOp "==" Equals Ex.AssocLeft
    ],
    [
      infixOp "&&" And Ex.AssocLeft,
      infixOp "||" Or Ex.AssocLeft
    ]
  ]

pkgDecl :: Parser PackageDeclaration
pkgDecl = do
  si <- currentSourceInfo
  reserved "package"
  name <- packageName
  topSeparator
  return (PackageDeclaration si name)

topLevel :: Parser TopLevel
topLevel = import' <|> try typeSignature <|> def
  where
  tvarBinding = do
    si <- currentSourceInfo
    v <- tvar
    return (TVarBindingExpr si v)
  explicitlyQuantifiedType = do
    si <- currentSourceInfo
    reservedOp "forall"
    bindings <- many1 tvarBinding
    reserved "."
    Explicit si bindings <$> type'
  implicitlyQuantifiedType = do
    si <- currentSourceInfo
    Implicit si <$> type'
  signature = try explicitlyQuantifiedType <|> implicitlyQuantifiedType
  typeSignature = do
    si <- currentSourceInfo
    i <- identifier
    reservedOp "::"
    TypeSignature si i <$> signature
  valueDef = do
    si <- currentSourceInfo
    i <- identifier
    reservedOp "="
    ValueDefinition si i <$> expr
  fnDef = do
    si <- currentSourceInfo
    FnDefinition si <$> identifier <*> many binding <*> (rArrow *> expr)
  def = try valueDef <|> fnDef
  import' = do
    si <- currentSourceInfo
    reserved "import"
    ImportDeclaration si <$> importName

package :: Parser Package
package = Package <$> pkgDecl <*> topLevel `sepBy` topSeparator

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: L.Text -> Either ParseError TopLevel
parseTopLevel = parse (contents topLevel) "<stdin>"

parsePackage ::  FilePath -> L.Text -> Either ParseError Package
parsePackage = parse (contents package)
