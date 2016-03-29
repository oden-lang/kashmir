-- | A representation of a subset of the Go Programming Language type system,
-- based on https://golang.org/ref/spec. Types not needed in Oden are excluded.
module Oden.Go.Type where

import Oden.Go.Identifier

data StructField = StructField Identifier Type
                 deriving (Show, Eq)

data Returns = Returns [Type]
             deriving (Show, Eq)

data Parameters = Parameters [Type] Bool
                deriving (Show, Eq)

data InterfaceMethodSpec = Method Identifier Parameters Returns
                         | Embed Identifier
                         deriving (Show, Eq)

data Type = Basic Identifier Bool
          | Pointer Type
          | Array Int Type
          | Slice Type
          | Signature (Maybe Type) Parameters Returns
          | Struct [StructField]
          | Named [String] Identifier Type
          | Interface [InterfaceMethodSpec]
          | Unsupported String                        -- Temporary solution for the Importer.
          deriving (Show, Eq)

