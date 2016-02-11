module Oden.Core.Operator where

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Equals
                    | Concat
                    | LessThan
                    | GreaterThan
                    | LessThanEqual
                    | GreaterThanEqual
                    | And
                    | Or
                    deriving (Eq, Ord)

data UnaryOperator = Negate
                   deriving (Eq, Ord)

instance Show BinaryOperator where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Equals = "=="
  show Concat = "++"
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanEqual = "<="
  show GreaterThanEqual = ">="
  show And = "&&"
  show Or = "||"

instance Show UnaryOperator where
  show Negate = "-"
