import Data.Matrix

%default total

interface Texty a where
  toText : a -> String

data Tile
  = Wall
  | Empty
  | Player
  | Jackal

tileChar : Tile -> Char
tileChar Wall = '#'
tileChar Empty = '.'
tileChar Player = '@'
tileChar Jackal = 'J'

Board : (rows : Nat) -> (cols : Nat) -> Type
Board rows cols = Matrix rows cols Tile

Texty (Vect cols Tile) where
  toText xs = pack (map tileChar xs)

Texty (Board rows cols) where
  toText = concatMap $ \x => toText x ++ "\n"

mkRow : (cols : Nat) -> Vect cols Tile
mkRow Z
  = []
mkRow (S Z)
  = [Wall]
mkRow (S (S Z))
  = [Wall, Wall]
mkRow (S (S innerCols@(S k)))
  = rewrite plusCommutative 1 k in
    Wall :: replicate innerCols Empty ++ [Wall]

twoMoreThan : Nat -> Nat
twoMoreThan = S . S

mkBoard : (innerRows : Nat) ->
          (innerCols : Nat) ->
          (Board (twoMoreThan innerRows) (twoMoreThan innerCols))
mkBoard Z innerCols
  = replicate 2 (replicate _ Wall)
mkBoard innerRows Z
  = replicate _ [Wall, Wall]
mkBoard innerRows@(S k) innerCols@(S _)
  = rewrite plusCommutative 1 k in
     replicate _ Wall
  :: replicate innerRows (mkRow _)
  ++ [replicate _ Wall]

main : IO ()
main = putStr $ toText (mkBoard 10 10)

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
