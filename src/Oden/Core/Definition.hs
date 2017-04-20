{-# LANGUAGE LambdaCase #-}
module Oden.Core.Definition where

import           Oden.Core.Expr
import           Oden.Core.ProtocolImplementation

import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo

import           Oden.Type.Polymorphic

data Definition e
  = Definition (Metadata SourceInfo) QualifiedName (Scheme, e)
  | ForeignDefinition (Metadata SourceInfo) QualifiedName Scheme
  | TypeDefinition (Metadata SourceInfo) QualifiedName [NameBinding] Type
  | ProtocolDefinition (Metadata SourceInfo) QualifiedName Protocol
  | Implementation (Metadata SourceInfo) (ProtocolImplementation e)
  deriving (Show, Eq, Ord)

instance HasSourceInfo (Definition e) where
  getSourceInfo =
    \case
      Definition (Metadata si) _ _         -> si
      ForeignDefinition (Metadata si) _ _  -> si
      TypeDefinition (Metadata si) _ _ _   -> si
      ProtocolDefinition (Metadata si) _ _ -> si
      Implementation (Metadata si) _       -> si

  setSourceInfo si =
    \case
      Definition _ q se        -> Definition (Metadata si) q se
      ForeignDefinition _ q s  -> ForeignDefinition (Metadata si) q s
      TypeDefinition _ q nb t  -> TypeDefinition (Metadata si) q nb t
      ProtocolDefinition _ q p -> ProtocolDefinition (Metadata si) q p
      Implementation _ p       -> Implementation (Metadata si) p

