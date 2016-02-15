module Oden.Pretty where

import           Oden.Core
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Type.Basic
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

import           Text.PrettyPrint

class Pretty e where
  pp :: e -> Doc

commaSepParens :: Pretty p => [p] -> Doc
commaSepParens ps = parens (hcat (punctuate (text ", ") (map pp ps)))

rArr :: Doc
rArr = text "->"

instance Pretty Binding where
  pp (Binding _ name) = text name

instance Pretty UnaryOperator where
  pp Positive = text "+"
  pp Negative = text "-"
  pp Not      = text "!"

instance Pretty BinaryOperator where
  pp Add = text "+"
  pp Subtract = text "-"
  pp Multiply = text "*"
  pp Divide = text "/"
  pp Equals = text "=="
  pp Concat = text "++"
  pp LessThan = text "<"
  pp GreaterThan = text ">"
  pp LessThanEqual = text "<="
  pp GreaterThanEqual = text ">="
  pp And = text "&&"
  pp Or = text "||"

instance Pretty Identifier where
  pp (Qualified p n) = text p <> text "." <> text n
  pp (Unqualified n) = text n

instance Pretty (Expr t) where
  pp (Symbol _ i _) = pp i
  pp (Subscript _ a i _) = pp a <> text "[" <> pp i <> text "]"
  pp (UnaryOp _ op e _) = pp op <+> pp e
  pp (BinaryOp _ op e1 e2 _) = pp e1 <+> pp op <+> pp e2
  pp (Application _ f a _) = pp f <> text "(" <> pp a <> text ")"
  pp (NoArgApplication _ f _) = pp f <> text "()"
  pp (UncurriedFnApplication _ f as _) = pp f <> commaSepParens as
  pp (Fn _ (Binding _ n) b _) = text "fn" <+> text n <+> rArr <+> pp b
  pp (NoArgFn _ b _) = text "fn" <+> rArr <+> pp b
  pp (Let _ (Binding _ n) e b _) =
    text "let" <+> text n <+> equals <+> pp e <+> text "in" <+> pp b
  pp (Literal _ (Int n) _) = integer n
  pp (Literal _ (Bool True) _) = text "true"
  pp (Literal _ (Bool False) _) = text "false"
  pp (Literal _ (String s) _) = text (show s)
  pp (Literal _ Unit{} _) = text "()"
  pp (Tuple _ f s r _) = commaSepParens (f:s:r)
  pp (If _ c t e _) =
    text "if" <+> pp c <+> text "then" <+> pp t <+> text "else" <+> pp e
  pp (Slice _ es _) =
    text "[]" <> braces (hcat (punctuate (text ", ") (map pp es)))
  pp (Block _ es _) =
    braces (vcat (map pp es))

instance Pretty Poly.TVar where
  pp (Poly.TV s) = text ('#' : s)

instance Pretty Poly.TVarBinding where
  pp (Poly.TVarBinding _ v) = pp v

instance Pretty Poly.Type where
  pp (Poly.TAny _) = text "any"
  pp (Poly.TBasic _ TInt) = text "int"
  pp (Poly.TBasic _ TString) = text "string"
  pp (Poly.TBasic _ TBool) = text "bool"
  pp (Poly.TUnit _) = text "()"
  pp (Poly.TTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pp (f:s:r))))
  pp (Poly.TVar _ v) = pp v
  pp (Poly.TCon _ s) = text s
  pp (Poly.TNoArgFn _ t) = rArr <+> pp t
  pp (Poly.TFn _ tf ta) = pp tf <+> rArr <+> pp ta
  pp (Poly.TUncurriedFn _ as r) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> pp r
  pp (Poly.TVariadicFn _ as v r) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> pp r
  pp (Poly.TSlice _ t) =
    text "[]" <> braces (pp t)

instance Pretty Poly.Scheme where
  pp (Poly.Forall _ vs t) = text "forall" <+> hsep (map pp vs) <> text "." <+> pp t

instance Pretty Mono.Type where
  pp (Mono.TAny _) = text "any"
  pp (Mono.TBasic _ TInt) = text "int"
  pp (Mono.TBasic _ TString) = text "string"
  pp (Mono.TBasic _ TBool) = text "bool"
  pp (Mono.TUnit _) = text "()"
  pp (Mono.TTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pp (f:s:r))))
  pp (Mono.TCon _ s) = text s
  pp (Mono.TNoArgFn _ t) = rArr <+> pp t
  pp (Mono.TFn _ tf ta) = pp tf <+> rArr <+> pp ta
  pp (Mono.TUncurriedFn _ as r) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> pp r
  pp (Mono.TVariadicFn _ as v r) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> pp r
  pp (Mono.TSlice _ t) =
    text "!" <> braces (pp t)
