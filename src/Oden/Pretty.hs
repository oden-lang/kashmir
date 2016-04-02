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

import           Data.List             (intersperse)
import           Data.Set              (toList)

import           Text.PrettyPrint.Leijen

commaSepParens :: Pretty p => [p] -> Doc
commaSepParens ps = parens (hcat (punctuate (text ", ") (map pretty ps)))

rArr :: Doc
rArr = text "->"

instance Pretty Untyped.NameBinding where
  pretty (Untyped.NameBinding _ identifier) = pretty identifier

instance Pretty Untyped.Range where
  pretty (Untyped.Range e1 e2) = brackets $ pretty e1 <+> text ":" <+> pretty e2
  pretty (Untyped.RangeTo e) = brackets $ text ":" <+> pretty e
  pretty (Untyped.RangeFrom e) = brackets $ pretty e <+> text ":"

instance Pretty Untyped.FieldInitializer where
  pretty (Untyped.FieldInitializer _ label expr) = pretty label <+> text "=" <+> pretty expr

instance Pretty Untyped.Expr where
  pretty (Untyped.Symbol _ i) = pretty i
  pretty (Untyped.Subscript _ s i) = pretty s <> text "[" <> pretty i <> text "]"
  pretty (Untyped.Subslice _ s r) = pretty s <> pretty r
  pretty (Untyped.UnaryOp _ op e) = pretty op <+> pretty e
  pretty (Untyped.BinaryOp _ op e1 e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (Untyped.Application _ f a) = pretty f <> commaSepParens a
  pretty (Untyped.Fn _ n b) = parens (pretty n) <+> rArr <+> pretty b
  pretty (Untyped.NoArgFn _ b) = parens empty <+> rArr <+> pretty b
  pretty (Untyped.Let _ n e b) =
    text "let" <+> pretty n <+> equals <+> pretty e <+> text "in" <+> pretty b
  pretty (Untyped.Literal _ (Untyped.Int n)) = integer n
  pretty (Untyped.Literal _ (Untyped.Bool True)) = text "true"
  pretty (Untyped.Literal _ (Untyped.Bool False)) = text "false"
  pretty (Untyped.Literal _ (Untyped.String s)) = text (show s)
  pretty (Untyped.Literal _ Untyped.Unit{}) = text "()"
  pretty (Untyped.Tuple _ f s r) = commaSepParens (f:s:r)
  pretty (Untyped.If _ c t e) =
    text "if" <+> pretty c <+> text "then" <+> pretty t <+> text "else" <+> pretty e
  pretty (Untyped.Slice _ es) =
    text "[]" <> braces (hcat (punctuate (text ", ") (map pretty es)))
  pretty (Untyped.Block _ es) =
    braces (vcat (map pretty es))
  pretty (Untyped.RecordInitializer _ fields) =
    braces (hcat (punctuate (text ", ") (map pretty fields)))
  pretty (Untyped.MemberAccess _ pkgAlias name) =
    pretty pkgAlias <> text "." <> pretty name

instance Pretty NameBinding where
  pretty (NameBinding _ identifier) = pretty identifier

instance Pretty UnaryOperator where
  pretty Positive = text "+"
  pretty Negative = text "-"
  pretty Not      = text "!"

instance Pretty BinaryOperator where
  pretty Add = text "+"
  pretty Subtract = text "-"
  pretty Multiply = text "*"
  pretty Divide = text "/"
  pretty Equals = text "=="
  pretty Concat = text "++"
  pretty LessThan = text "<"
  pretty GreaterThan = text ">"
  pretty LessThanEqual = text "<="
  pretty GreaterThanEqual = text ">="
  pretty And = text "&&"
  pretty Or = text "||"

instance Pretty Identifier where
  pretty (Identifier n) = text n

instance Pretty t => Pretty (FieldInitializer t) where
  pretty (FieldInitializer _ l e) = pretty l <+> text "=" <+> pretty e

instance Pretty t => Pretty (Expr t) where
  pretty (Symbol _ i _) = pretty i
  pretty (Subscript _ s i _) = pretty s <> text "[" <> pretty i <> text "]"
  pretty (Subslice _ s r _) = pretty s <> pretty r
  pretty (UnaryOp _ op e _) = pretty op <+> pretty e
  pretty (BinaryOp _ op e1 e2 _) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (Application _ f a _) = pretty f <> text "(" <> pretty a <> text ")"
  pretty (NoArgApplication _ f _) = pretty f <> text "()"
  pretty (ForeignFnApplication _ f as _) = pretty f <> commaSepParens as
  pretty (Fn _ n b _) = parens (parens (pretty n) <+> rArr <+> pretty b)
  pretty (NoArgFn _ b _) = parens empty <+> rArr <+> pretty b
  pretty (Let _ n e b _) =
    text "let" <+> pretty n <+> equals <+> pretty e <+> text "in" <+> pretty b
  pretty (Literal _ (Int n) _) = integer n
  pretty (Literal _ (Bool True) _) = text "true"
  pretty (Literal _ (Bool False) _) = text "false"
  pretty (Literal _ (String s) _) = text (show s)
  pretty (Literal _ Unit{} _) = text "()"
  pretty (Tuple _ f s r _) = commaSepParens (f:s:r)
  pretty (If _ c t e _) =
    text "if" <+> pretty c <+> text "then" <+> pretty t <+> text "else" <+> pretty e
  pretty (Slice _ es _) =
    text "[]" <> braces (hcat (punctuate (text ", ") (map pretty es)))
  pretty (Block _ es _) =
    braces (vcat (map pretty es))
  pretty (RecordInitializer _ _ fields) =
    braces (hcat (punctuate (text ", ") (map pretty fields)))
  pretty (RecordFieldAccess _ expr name _) =
    pretty expr <> text "." <> pretty name
  pretty (PackageMemberAccess _ pkgAlias name _) =
    pretty pkgAlias <> text "." <> pretty name

instance Pretty r => Pretty (Range r) where
  pretty (Range e1 e2) = brackets $ pretty e1 <+> text ":" <+> pretty e2
  pretty (RangeTo e) = brackets $ text ":" <+> pretty e
  pretty (RangeFrom e) = brackets $ pretty e <+> text ":"

instance Pretty CanonicalExpr where
  pretty (scheme, expr) = pretty expr <+> text ":" <+> pretty scheme

instance Pretty Definition where
  pretty (Definition _ name (scheme, expr)) = vcat [
      pretty name <+> text ":" <+> pretty scheme,
      pretty name <+> text "=" <+> pretty expr
    ]
  pretty (ForeignDefinition _ name scheme) = vcat [
      text "// (foreign)",
      text "//" <+> pretty name <+> text ":" <+> pretty scheme
    ]
  pretty (TypeDefinition _ name _ type') =
    text "type" <+> pretty name <+> equals <+> pretty type'

instance Pretty PackageName where
  pretty parts = hcat (punctuate (text "/") (map text parts))

instance Pretty PackageDeclaration where
  pretty (PackageDeclaration _ name) = text "package" <+> pretty name

instance Pretty ImportedPackage where
  pretty (ImportedPackage _ _ (Package (PackageDeclaration _ pkgName) _ _)) =
    text "import" <+> pretty pkgName

instance Pretty Package where
  pretty (Package decl imports defs) =
    vcat (pretty decl : map pretty imports ++ map pretty defs)

instance Pretty Poly.TVar where
  pretty (Poly.TV s) = text s

instance Pretty Poly.TVarBinding where
  pretty (Poly.TVarBinding _ v) = pretty v

instance Pretty QualifiedName where
  pretty (FQN pkg identifier) = hcat (punctuate (text ".") (map text pkg ++ [pretty identifier]))

instance Pretty Poly.Type where
  pretty (Poly.TTuple _ f s r) = commaSepParens (f:s:r)
  pretty (Poly.TVar _ v) = pretty v
  pretty (Poly.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pretty (Poly.TCon _ n) = pretty n
  pretty (Poly.TNoArgFn _ t) = rArr <+> pretty t
  pretty (Poly.TFn _ tf ta) = pretty tf <+> rArr <+> pretty ta
  pretty (Poly.TForeignFn _ _ ps rs) = text "<foreign>" <+> hsep (intersperse (text "&") (map pretty ps)) <+> rArr <+> ppReturns rs
  pretty (Poly.TSlice _ t) =
    text "[]" <> braces (pretty t)
  pretty (Poly.TNamed _ n _) = pretty n
  pretty (Poly.TRecord _ r) = braces (ppFields r)
  pretty Poly.REmpty{} = braces empty
  pretty r@Poly.RExtension{} = parens (ppFields r)

ppFields :: Poly.Type -> Doc
ppFields r =
  case Poly.getLeafRow r of
    var@Poly.TVar{} -> printFields r <+> text "|" <+> pretty var
    _ -> printFields r
  where
  printFields = hcat . punctuate (text ", ") . map printField . Poly.rowToList
  printField (label, type') = pretty label <> colon <+> pretty type'

instance Pretty Poly.Scheme where
  pretty (Poly.Forall _ [] t) = pretty t
  pretty (Poly.Forall _ vs t) = text "forall" <+> hsep (map pretty vs) <> text "." <+> pretty t

ppMonoFields :: Mono.Type -> Doc
ppMonoFields Mono.REmpty{} = empty
ppMonoFields r = getPairs r
  where
  getPairs (Mono.RExtension _ label type' Mono.REmpty{}) =
    pretty label <> colon <+> pretty type'
  getPairs (Mono.RExtension _ label type' row) =
    pretty label <> colon <+> pretty type' <> comma <+> getPairs row
  getPairs _ = empty

ppReturns :: Pretty p => [p] -> Doc
ppReturns [r] = pretty r
ppReturns rs = commaSepParens rs

instance Pretty Mono.Type where
  pretty (Mono.TTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pretty (f:s:r))))
  pretty (Mono.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pretty (Mono.TCon _ n) = pretty n
  pretty (Mono.TNoArgFn _ t) = rArr <+> pretty t
  pretty (Mono.TFn _ tf ta) = pretty tf <+> rArr <+> pretty ta
  pretty (Mono.TForeignFn _ _ ps rs) = text "<foreign>" <+> hsep (intersperse (text "&") (map pretty ps)) <+> rArr <+> ppReturns rs
  pretty (Mono.TSlice _ t) =
    text "[]" <> braces (pretty t)
  pretty (Mono.TNamed _ n _) = pretty n
  pretty (Mono.TRecord _ r) = braces (ppMonoFields r)
  pretty Mono.REmpty{} = braces empty
  pretty r@Mono.RExtension{} = parens (ppMonoFields r)

instance Pretty (SignatureVarBinding a) where
  pretty (SignatureVarBinding _ s) = pretty s

instance Pretty (SignatureExpr a) where
  pretty (TSUnit _) = text "()"
  pretty (TSSymbol _ i) = pretty i
  pretty (TSApp _ d r) = pretty d <> parens (pretty r)
  pretty (TSNoArgFn _ t) = rArr <+> pretty t
  pretty (TSFn _ tf ta) = pretty tf <+> rArr <+> pretty ta
  pretty (TSTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pretty (f:s:r))))
  pretty (TSSlice _ t) =
    text "!" <> braces (pretty t)
  pretty (TSRowEmpty _) = empty
  pretty (TSRowExtension _ label type' TSRowEmpty{}) = pretty label <> colon <+> pretty type'
  pretty (TSRowExtension _ label type' row) = pretty label <> colon <+> pretty type' <> comma <+> pretty row
  -- TODO: Better printing with braces
  pretty (TSRecord _ row) = text "record " <> pretty row

instance Pretty (TypeSignature a) where
  pretty (TypeSignature _ [] expr) = pretty expr
  pretty (TypeSignature _ vars expr) =
    text "forall" <+> hsep (map pretty vars) <> text "." <+> pretty expr

instance Pretty InstantiatedDefinition where
  pretty (InstantiatedDefinition polyName _si name expr) = vcat [
      text "//" <+> pretty polyName,
      pretty name <+> text ":" <+> pretty (typeOf expr),
      pretty name <+> text "=" <+> pretty expr
    ]

instance Pretty MonomorphedDefinition where
  pretty (MonomorphedDefinition _ name _ expr) = vcat [
      pretty name <+> text ":" <+> pretty (typeOf expr),
      pretty name <+> text "=" <+> pretty expr
    ]

instance Pretty MonomorphedPackage where
  pretty (MonomorphedPackage decl imports is ms) =
    vcat (pretty decl
          : (map pretty imports)
          ++ (map pretty $ toList is)
          ++ (map pretty $ toList ms))
