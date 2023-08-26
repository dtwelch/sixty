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
	
{-
-- NOTE: they use :=> as the ctor name instead of Arr
data DSum tag f = forall a. Arr (tag a) (f a)
  
data Tag a where
  AString   :: Tag Text
  AnInt     :: Tag Int
  Rec      :: Tag (DSum Tag Identity)

(==>) :: 
-}