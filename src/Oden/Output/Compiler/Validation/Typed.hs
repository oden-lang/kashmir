module Oden.Output.Compiler.Validation.Typed where

import           Text.PrettyPrint.Leijen

import           Oden.Compiler.Validation.Typed
import           Oden.Core.Expr
import           Oden.Output
import           Oden.Pretty ()
import           Oden.SourceInfo

instance OdenOutput ValidationError where
  outputType _ = Error

  name ValueDiscarded{}                 = "Compiler.Validation.ValueDiscarded"
  name DivisionByZero{}                 = "Compiler.Validation.DivisionByZero"
  name NegativeSliceIndex{}             = "Compiler.Validation.NegativeSliceIndex"
  name InvalidSubslice{}                = "Compiler.Validation.InvalidSubslice"
  name UnusedImport{}                   = "Compiler.Validation.UnusedImport"
  name MainPkgDoesNotHaveMainFn{}       = "Compiler.Validation.MainPkgDoesNotHaveMainFn"
  name MainPkgDoesNotHaveValidMainFn{}  = "Compiler.Validation.MainPkgDoesNotHaveValidMainFn"

  header (ValueDiscarded e) s =
    text "Value of type"
    <+> code s (pretty (typeOf e))
    <+> text "discarded"
  header (DivisionByZero e) s =
    text "Division by zero: "
    <+> code s (pretty e)
  header (NegativeSliceIndex e) s =
    text "Negative index: "
    <+> code s (pretty e)
  header (InvalidSubslice _ r) s =
    text "Invalid subslice range "
    <+> code s (pretty r)
  header (UnusedImport _ _ pkg) s =
    text "Package" <+> code s (pretty pkg) <+> text "imported but not used"
  header (MainPkgDoesNotHaveMainFn _) _ =
    text "Package main does not have a main function"
  header (MainPkgDoesNotHaveValidMainFn _) _ =
    text "Package main does not have a main function with the valid signature"

  details ValueDiscarded{} _                 = empty
  details DivisionByZero{} _                 = empty
  details NegativeSliceIndex{} _             = text "Indices must be >= 0"
  details InvalidSubslice{} _                = text "Ranges must go from smaller to bigger"
  details UnusedImport{} _                   = empty
  details MainPkgDoesNotHaveMainFn{} _       = empty
  details MainPkgDoesNotHaveValidMainFn{} _  = text "Every main package must have main function with signature:  main : -> ()"


  sourceInfo (ValueDiscarded e)                 = Just (getSourceInfo e)
  sourceInfo (DivisionByZero e)                 = Just (getSourceInfo e)
  sourceInfo (NegativeSliceIndex e)             = Just (getSourceInfo e)
  sourceInfo (InvalidSubslice si _)             = Just si
  sourceInfo (UnusedImport si _ _)              = Just si
  sourceInfo (MainPkgDoesNotHaveMainFn si)      = Just si
  sourceInfo (MainPkgDoesNotHaveValidMainFn si) = Just si


instance OdenOutput ValidationWarning where
  outputType _ = Warning

  name _     = "Compiler.Validation.Warning"

  header _ _ = empty

  details _ _ = empty

  sourceInfo _ = Nothing
