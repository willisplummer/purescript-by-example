module Main where

import Control.Monad.Eff.Console (logShow)
import Math (sqrt, pi)
import Prelude

circleArea r = pi * r * r

diagonal w h = sqrt (w * w + h * h)

main = logShow (diagonal 3.0 4.0)
