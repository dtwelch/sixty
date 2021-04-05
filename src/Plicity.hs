{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
module Plicity where

import Protolude

import Data.Text.Prettyprint.Doc
import Data.Persist

data Plicity
  = Implicit
  | Explicit
  | Constraint
  deriving (Eq, Ord, Show, Generic, Persist, Hashable)

instance Pretty Plicity where
  pretty plicity =
    case plicity of
      Implicit ->
        "implicit"

      Explicit ->
        "explicit"

      Constraint ->
        "constraint"

isImplicitish :: Plicity -> Bool
isImplicitish plicity =
  case plicity of
    Implicit ->
      True

    Explicit ->
      False

    Constraint ->
      True

prettyAnnotation :: Plicity -> Doc ann
prettyAnnotation plicity =
  case plicity of
    Implicit ->
      "@"

    Explicit ->
      ""

    Constraint ->
      "!"

implicitise :: Plicity -> Plicity
implicitise plicity =
  case plicity of
    Explicit ->
      Implicit

    Implicit ->
      Implicit

    Constraint ->
      Constraint
