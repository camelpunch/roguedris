module Random

import Data.Fin

%default total

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

export
diceRolls : (seed : Int) -> Stream (Fin 12)
diceRolls = map (restrict _ . cast) . randoms
