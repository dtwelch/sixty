{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Literal where

import Data.Persist
import Prettyprinter
import Protolude

newtype Literal
  = Integer Integer
  deriving (Eq, Generic, Show, Hashable, Persist)

instance Pretty Literal where
  pretty literal =
    case literal of
      Literal.Integer int ->
        pretty int
