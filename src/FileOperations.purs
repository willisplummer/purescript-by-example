module FileOperations where

import Prelude

import Data.Array (null)
import Data.Int (even)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isInt :: Int -> Boolean
isInt = even
