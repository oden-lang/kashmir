{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Oden.Pretty where

import           Oden.Core
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Compiler.Monomorphization
import           Oden.QualifiedName    (QualifiedName(..))
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly
import           Oden.Type.Signature

import           Data.Set              (toList)

import           Text.PrettyPrint

class Pretty e where
  pp :: e -> Doc

commaSepParens :: Pretty p => [p] -> Doc
commaSepParens ps = parens (hcat (punctuate (text ", ") (map pp ps)))

rArr :: Doc
rArr = text "->"

instance Pretty Untyped.NameBinding where
  pp (Untyped.NameBinding _ identifier) = pp identifier

instance Pretty Untyped.Range where
  pp (Untyped.Range e1 e2) = brackets $ pp e1 <+> text ":" <+> pp e2
  pp (Untyped.RangeTo e) = brackets $ text ":" <+> pp e
  pp (Untyped.RangeFrom e) = brackets $ pp e <+> text ":"

instance Pretty Untyped.FieldInitializer where
  pp (Untyped.FieldInitializer _ label expr) = pp label <+> text "=" <+> pp expr

instance Pretty Untyped.Expr where
  pp (Untyped.Symbol _ i) = pp i
  pp (Untyped.Subscript _ s i) = pp s <> text "[" <> pp i <> text "]"
  pp (Untyped.Subslice _ s r) = pp s <> pp r
  pp (Untyped.UnaryOp _ op e) = pp op <+> pp e
  pp (Untyped.BinaryOp _ op e1 e2) = parens (pp e1 <+> pp op <+> pp e2)
  pp (Untyped.Application _ f a) = pp f <> commaSepParens a
  pp (Untyped.Fn _ n b) = parens (pp n) <+> rArr <+> pp b
  pp (Untyped.NoArgFn _ b) = parens empty <+> rArr <+> pp b
  pp (Untyped.Let _ n e b) =
    text "let" <+> pp n <+> equals <+> pp e <+> text "in" <+> pp b
  pp (Untyped.Literal _ (Untyped.Int n)) = integer n
  pp (Untyped.Literal _ (Untyped.Bool True)) = text "true"
  pp (Untyped.Literal _ (Untyped.Bool False)) = text "false"
  pp (Untyped.Literal _ (Untyped.String s)) = text (show s)
  pp (Untyped.Literal _ Untyped.Unit{}) = text "()"
  pp (Untyped.Tuple _ f s r) = commaSepParens (f:s:r)
  pp (Untyped.If _ c t e) =
    text "if" <+> pp c <+> text "then" <+> pp t <+> text "else" <+> pp e
  pp (Untyped.Slice _ es) =
    text "[]" <> braces (hcat (punctuate (text ", ") (map pp es)))
  pp (Untyped.Block _ es) =
    braces (vcat (map pp es))
  pp (Untyped.RecordInitializer _ fields) =
    braces (hcat (punctuate (text ", ") (map pp fields)))
  pp (Untyped.MemberAccess _ pkgAlias name) =
    pp pkgAlias <> text "." <> pp name

instance Pretty NameBinding where
  pp (NameBinding _ identifier) = pp identifier

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
  pp (Identifier n) = text n

instance Pretty t => Pretty (FieldInitializer t) where
  pp (FieldInitializer _ l e) = pp l <+> text "=" <+> pp e

instance Pretty t => Pretty (Expr t) where
  pp (Symbol _ i _) = pp i
  pp (Subscript _ s i _) = pp s <> text "[" <> pp i <> text "]"
  pp (Subslice _ s r _) = pp s <> pp r
  pp (UnaryOp _ op e _) = pp op <+> pp e
  pp (BinaryOp _ op e1 e2 _) = parens (pp e1 <+> pp op <+> pp e2)
  pp (Application _ f a _) = pp f <> text "(" <> pp a <> text ")"
  pp (NoArgApplication _ f _) = pp f <> text "()"
  pp (UncurriedFnApplication _ f as _) = pp f <> commaSepParens as
  pp (Fn _ n b _) = parens (pp n) <+> rArr <+> pp b
  pp (NoArgFn _ b _) = parens empty <+> rArr <+> pp b
  pp (Let _ n e b _) =
    text "let" <+> pp n <+> equals <+> pp e <+> text "in" <+> pp b
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
  pp (RecordInitializer _ _ fields) =
    braces (hcat (punctuate (text ", ") (map pp fields)))
  pp (RecordFieldAccess _ expr name _) =
    pp expr <> text "." <> pp name
  pp (PackageMemberAccess _ pkgAlias name _) =
    pp pkgAlias <> text "." <> pp name

instance Pretty r => Pretty (Range r) where
  pp (Range e1 e2) = brackets $ pp e1 <+> text ":" <+> pp e2
  pp (RangeTo e) = brackets $ text ":" <+> pp e
  pp (RangeFrom e) = brackets $ pp e <+> text ":"

instance Pretty CanonicalExpr where
  pp (scheme, expr) = pp expr <+> text ":" <+> pp scheme

instance Pretty Definition where
  pp (Definition _ name (scheme, expr)) =
    pp name <+> text ":" <+> pp scheme $+$
    pp name <+> text "=" <+> pp expr
  pp (ForeignDefinition _ name scheme) =
    text "// (foreign)" $+$
    text "//" <+> pp name <+> text ":" <+> pp scheme
  pp (TypeDefinition _ name _ type') =
    text "type" <+> pp name <+> equals <+> pp type'

instance Pretty PackageName where
  pp parts = hcat (punctuate (text "/") (map text parts))

instance Pretty PackageDeclaration where
  pp (PackageDeclaration _ name) = text "package" <+> pp name

instance Pretty ImportedPackage where
  pp (ImportedPackage _ _ (Package (PackageDeclaration _ pkgName) _ _)) =
    text "import" <+> pp pkgName

instance Pretty Package where
  pp (Package decl imports defs) =
    pp decl $+$
    vcat (map pp imports) $+$
    vcat (map pp defs)

instance Pretty Poly.TVar where
  pp (Poly.TV s) = text s

instance Pretty Poly.TVarBinding where
  pp (Poly.TVarBinding _ v) = pp v

instance Pretty QualifiedName where
  pp (FQN pkg identifier) = hcat (punctuate (text ".") (map text pkg ++ [pp identifier]))

instance Pretty Poly.Type where
  pp (Poly.TAny _) = text "any"
  pp (Poly.TTuple _ f s r) = commaSepParens (f:s:r)
  pp (Poly.TVar _ v) = pp v
  pp (Poly.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pp (Poly.TCon _ n) = pp n
  pp (Poly.TNoArgFn _ t) = rArr <+> pp t
  pp (Poly.TFn _ tf ta) = pp tf <+> rArr <+> pp ta
  pp (Poly.TUncurriedFn _ as [r]) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> pp r
  pp (Poly.TUncurriedFn _ as rs) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> braces (hsep (punctuate (text "&") (map pp rs)))
  pp (Poly.TVariadicFn _ as v [r]) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> pp r
  pp (Poly.TVariadicFn _ as v rs) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> braces (hsep (punctuate (text "&") (map pp rs)))
  pp (Poly.TSlice _ t) =
    text "[]" <> braces (pp t)
  pp (Poly.TNamed _ n _) = pp n
  pp (Poly.TRecord _ r) = braces (ppFields r)
  pp Poly.REmpty{} = braces empty
  pp r@Poly.RExtension{} = parens (ppFields r)

ppFields :: Poly.Type -> Doc
ppFields r =
  case Poly.getLeafRow r of
    var@Poly.TVar{} -> printFields r <+> text "|" <+> pp var
    _ -> printFields r
  where
  printFields = hcat . punctuate (text ", ") . map printField . Poly.rowToList
  printField (label, type') = pp label <> colon <+> pp type'

instance Pretty Poly.Scheme where
  pp (Poly.Forall _ [] t) = pp t
  pp (Poly.Forall _ vs t) = text "forall" <+> hsep (map pp vs) <> text "." <+> pp t

ppMonoFields :: Mono.Type -> Doc
ppMonoFields Mono.REmpty{} = empty
ppMonoFields r = getPairs r
  where
  getPairs (Mono.RExtension _ label type' Mono.REmpty{}) =
    pp label <> colon <+> pp type'
  getPairs (Mono.RExtension _ label type' row) =
    pp label <> colon <+> pp type' <> comma <+> getPairs row
  getPairs _ = empty

instance Pretty Mono.Type where
  pp (Mono.TAny _) = text "any"
  pp (Mono.TTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pp (f:s:r))))
  pp (Mono.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pp (Mono.TCon _ n) = pp n
  pp (Mono.TNoArgFn _ t) = rArr <+> pp t
  pp (Mono.TFn _ tf ta) = pp tf <+> rArr <+> pp ta
  pp (Mono.TUncurriedFn _ as [r]) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> pp r
  pp (Mono.TUncurriedFn _ as rs) = hsep (punctuate (text "&") (map pp as)) <+> rArr <+> parens (hsep (punctuate (text ", ") (map pp rs)))
  pp (Mono.TVariadicFn _ as v [r]) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> pp r
  pp (Mono.TVariadicFn _ as v rs) = hsep (punctuate (text "&") (map pp as ++ [pp v <> text "*"])) <+> rArr <+> parens (hsep (punctuate (text ", ") (map pp rs)))
  pp (Mono.TSlice _ t) =
    text "!" <> braces (pp t)
  pp (Mono.TNamed _ n _) = pp n
  pp (Mono.TRecord _ r) = braces (ppMonoFields r)
  pp Mono.REmpty{} = braces empty
  pp r@Mono.RExtension{} = parens (ppMonoFields r)

instance Pretty (SignatureVarBinding a) where
  pp (SignatureVarBinding _ s) = pp s

instance Pretty (SignatureExpr a) where
  pp (TSUnit _) = text "()"
  pp (TSSymbol _ i) = pp i
  pp (TSApp _ d r) = pp d <> parens (pp r)
  pp (TSNoArgFn _ t) = rArr <+> pp t
  pp (TSFn _ tf ta) = pp tf <+> rArr <+> pp ta
  pp (TSTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pp (f:s:r))))
  pp (TSSlice _ t) =
    text "!" <> braces (pp t)
  pp (TSRowEmpty _) = empty
  pp (TSRowExtension _ label type' TSRowEmpty{}) = pp label <> colon <+> pp type'
  pp (TSRowExtension _ label type' row) = pp label <> colon <+> pp type' <> comma <+> pp row
  -- TODO: Better printing with braces
  pp (TSRecord _ row) = text "record " <> pp row

instance Pretty (TypeSignature a) where
  pp (TypeSignature _ [] expr) = pp expr
  pp (TypeSignature _ vars expr) =
    text "forall" <+> hsep (map pp vars) <> text "." <+> pp expr

instance Pretty InstantiatedDefinition where
  pp (InstantiatedDefinition polyName _si name expr) =
    text "//" <+> pp polyName $+$
    pp name <+> text ":" <+> pp (typeOf expr) $+$
    pp name <+> text "=" <+> pp expr

instance Pretty MonomorphedDefinition where
  pp (MonomorphedDefinition _ name _ expr) =
    pp name <+> text ":" <+> pp (typeOf expr) $+$
    pp name <+> text "=" <+> pp expr

instance Pretty MonomorphedPackage where
  pp (MonomorphedPackage decl imports is ms) =
    pp decl $+$
    vcat (map pp imports) $+$
    vcat (map pp $ toList is) $+$
    vcat (map pp $ toList ms)
