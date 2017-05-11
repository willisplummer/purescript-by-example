module Path where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Math (abs, remainder)
import Partial.Unsafe (unsafePartial)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEven :: Number -> Boolean
isEven 0.0 = true
isEven 1.0 = false
isEven n =
  if n < 0.0
    then isEven $ abs n
    else isEven $ n - 2.0


countEvens :: Array Number -> Int
countEvens arr =
  if null arr
    then 0
    else checkEven (unsafePartial head arr) + countEvens (unsafePartial tail arr)
  where
    checkEven :: Number -> Int
    checkEven n = 
      if isEven n
        then 1
        else 0
