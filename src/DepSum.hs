{-# LANGUAGE GADTs #-}

module DepSum where
import Data.String (String)
import Prelude (show, Int, map)

-- this module is just messing around with dependent sums based on this implementation:
-- https://github.com/obsidiansystems/dependent-sum/tree/master


-- NOTE: they use :=> as the ctor name instead of Arr
data DSum tag f = forall a. Arr (tag a) (f a)
 --- Arr : \forall a . (tag a)
--(==>) :: Applicative f => tag a -> a -> DSum tag f 
--(k :: tag a) ==> v = Arr k (pure v)

-- see for applicative when time comes: https://typelevel.org/cats/typeclasses/applicative.html
data Tag a where
  StringKey :: Tag String
  IntKey    :: Tag Int

toString :: DSum Tag [] -> [String]
toString (Arr StringKey strs) = strs
toString (Arr IntKey ints) = map show ints


-- >>> :t Arr StringKey (Identity (Txt.pack "hello!"))
-- Arr StringKey (Identity (Txt.pack "hello!")) :: DSum Tag Identity

