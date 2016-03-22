module Oden.Infer.Fixtures where

import           Oden.Core              as Core
import           Oden.Core.Operator
import qualified Oden.Core.Untyped      as Untyped
import           Oden.Environment       hiding (map)
import           Oden.Identifier
import           Oden.Infer.Environment as IE
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic
import           Oden.Type.Signature

import qualified Data.Set as Set

missing :: Metadata SourceInfo
missing = Metadata Missing

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

tvA = TV "a"
tvB = TV "b"
tvC = TV "c"
tvD = TV "d"

tvarA = TVar predefined tvA
tvarB = TVar predefined tvB
tvarC = TVar predefined tvC
tvarD = TVar predefined tvD

typeAny = TAny missing

scheme:: Type -> Scheme
scheme t = Forall (Metadata Predefined) (map (TVarBinding missing) $ Set.toList (ftv t)) t

typeFn :: Type -> Type -> Type
typeFn = TFn missing

named :: String -> Type -> Type
named = TNamed missing . FQN [] . Identifier

typeSlice :: Type -> Type
typeSlice = TSlice missing

intSlice = typeSlice typeInt

typeNoArgFn = TNoArgFn missing
typeUncurried = TUncurriedFn missing
typeVariadic = TVariadicFn missing

typeRecord = TRecord missing
emptyRow = REmpty missing
rowExt = RExtension missing

tsUnit = TSUnit Missing
tsVar = TSSymbol Missing . Identifier
tsSymbol = TSSymbol Missing
tsFn = TSFn Missing
tsRecord = TSRecord Missing
tsRowEmpty = TSRowEmpty Missing
tsRowExt = TSRowExtension Missing

implicit = TypeSignature Missing []
explicit = TypeSignature Missing
varBinding = SignatureVarBinding Missing . Identifier

forall = Forall missing
tvarBinding = TVarBinding missing

uSymbol                 = Untyped.Symbol missing
uOp                     = Untyped.BinaryOp missing
uApplication            = Untyped.Application missing
uFn                     = Untyped.Fn missing
uNoArgFn                = Untyped.NoArgFn missing
uLet                    = Untyped.Let missing
uLiteral                = Untyped.Literal missing
uTuple                  = Untyped.Tuple missing
uIf                     = Untyped.If missing
uSlice                  = Untyped.Slice missing
uBlock                  = Untyped.Block missing
uMemberAccess           = Untyped.MemberAccess missing

uInt    = Untyped.Int
uString = Untyped.String
uBool   = Untyped.Bool
uUnit   = Untyped.Unit

uNameBinding = Untyped.NameBinding missing
uDefinition = Untyped.Definition missing

tSymbol                 = Symbol missing
tOp                     = BinaryOp missing
tApplication            = Application missing
tNoArgApplication       = NoArgApplication missing
tUncurriedFnApplication = UncurriedFnApplication missing
tFn                     = Fn missing
tNoArgFn                = NoArgFn missing
tLet                    = Let missing
tLiteral                = Literal missing
tTuple                  = Tuple missing
tIf                     = If missing
tSlice                  = Slice missing
tBlock                  = Block missing
tFieldAccess            = RecordFieldAccess missing
tPackageMemberAcccess   = PackageMemberAccess missing

tUnit   = Unit
tInt    = Int
tString = String
tBool   = Bool

tDefinition = Definition missing
tNameBinding = NameBinding missing

predef :: TypingEnvironment
predef = fromPackage universe

predefAndStringLength :: TypingEnvironment
predefAndStringLength =  predef `extend` (Identifier "stringLength",
                                          Local predefined (Identifier "stringLength") $ forall [] (typeFn typeString typeInt))

predefAndMax :: TypingEnvironment
predefAndMax =  predef `extend` (Identifier "max",
                                 Local predefined (Identifier "max") $ forall [] (typeUncurried [typeInt, typeInt] [typeInt]))

predefAndMaxVariadic :: TypingEnvironment
predefAndMaxVariadic = predef `extend` (Identifier "max",
                                        Local predefined (Identifier "max") $ forall [] (typeVariadic [] typeInt [typeInt]))

predefAndIdentityAny :: TypingEnvironment
predefAndIdentityAny = predef `extend` (Identifier "identity",
                                        Local predefined (Identifier "identity") $ forall [] (typeUncurried [typeAny] [typeAny]))

fooBarPkgEnv :: TypingEnvironment
fooBarPkgEnv = predef `extend` (Identifier "foo",
                                IE.Package missing
                                (Identifier "foo")
                                (fromList [(Identifier "Bar",
                                            Local predefined (Identifier "Bar") $ forall [] typeInt)]))

booleanOp :: Type
booleanOp = typeFn typeBool (typeFn typeBool typeBool)

countToZero :: Untyped.Expr
countToZero =
  uFn
  (uNameBinding (Identifier "x"))
  (uIf
   (uOp
    Equals
    (uSymbol (Identifier "x"))
    (uLiteral (uInt 0)))
   (uLiteral (uInt 0))
   (uApplication
    (uSymbol (Identifier "f"))
    [uOp
     Subtract
     (uSymbol (Identifier "x"))
     (uLiteral (uInt 1))]))

intToInt :: Type
intToInt = typeFn typeInt typeInt

intToIntToInt :: Type
intToIntToInt = typeFn typeInt (typeFn typeInt typeInt)

countToZeroTyped :: Definition
countToZeroTyped =
  tDefinition
   (Identifier "f")
   (forall [] (typeFn typeInt typeInt),
    tFn
    (tNameBinding (Identifier "x"))
    (tIf
     (tOp
      Equals
      (tSymbol (Identifier "x") typeInt)
      (tLiteral (tInt 0) typeInt)
      typeBool)
     (tLiteral (tInt 0) typeInt)
     (tApplication
      (tSymbol (Identifier "f") intToInt)
      (tOp
       Subtract
       (tSymbol (Identifier "x") typeInt)
       (tLiteral (tInt 1) typeInt)
       typeInt)
      typeInt)
     typeInt)
    intToInt)

twiceUntyped :: Untyped.Expr
twiceUntyped =
  uFn
  (uNameBinding (Identifier "f"))
  (uFn
   (uNameBinding (Identifier "x"))
   (uApplication
     (uSymbol (Identifier "f"))
     [uApplication
     (uSymbol (Identifier "f"))
     [uSymbol (Identifier "x")]]))

twiceTyped :: Definition
twiceTyped =
  tDefinition (Identifier "twice") (forall [tvarBinding tvA] (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)),
                           tFn
                           (tNameBinding (Identifier "f"))
                           (tFn
                           (tNameBinding (Identifier "x"))
                           (tApplication
                             (tSymbol (Identifier "f") (typeFn tvarA tvarA))
                             (tApplication
                             (tSymbol (Identifier "f") (typeFn tvarA tvarA))
                             (tSymbol (Identifier "x") tvarA)
                             tvarA)
                             tvarA)
                           (typeFn tvarA tvarA))
                           (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)))
