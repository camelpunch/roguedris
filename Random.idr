module Random

import Data.Fin
import Data.Primitives.Views

%default total

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

bounded : Int -> Int
bounded num with (divides num 12)
  bound ((_ * div) + rem) | (DivBy prf) = rem + 1

export
diceRolls : (seed : Int) ->
            Stream (Fin 12)
diceRolls seed = map (toFin . cast . bounded) (randoms seed) where
  toFin : Integer -> Fin 12
  toFin n = fromMaybe 0 $
            integerToFin n _
