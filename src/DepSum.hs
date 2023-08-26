{-# LANGUAGE GADTs #-}

module DepSum where

import qualified Data.Char as Char
import Data.Coerce

import Debug.Trace

import Data.Text.Array (Array)

import qualified Data.Text as Txt
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Text
import qualified Data.Text.Internal.Encoding.Utf16 as Utf16
import qualified Data.Text.Internal.Unsafe.Char as Char
import qualified Position
import Protolude hiding (State, ord, state, trace)
import qualified Span
import qualified UTF16

-- this module is just messing around with dependent sums based on this implementation:
-- https://github.com/obsidiansystems/dependent-sum/tree/master

-- NOTE: they use :=> as the ctor name instead of Arr
data DSum tag f = forall a. Arr (tag a) (f a)

(==>) :: Applicative f => tag a -> a -> DSum tag f 
(k :: tag a) ==> v = Arr k (pure v)




-- >>> Arr AString (Identity (Txt.pack "hello!"))
-- No instance for (Show (DSum Tag Identity))
--   arising from a use of `evalPrint'
-- There are instances for similar types:
--   instance [safe] forall k (tag :: k -> Type) (f :: k -> Type).
--                   (GShow tag, Has' Show tag f) =>
--                   Show (DSum tag f)
--     -- Defined in `Data.Dependent.Sum'
-- In a stmt of an interactive GHCi command: evalPrint it_a6nJ1

