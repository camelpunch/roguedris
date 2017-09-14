import Data.Matrix

%default total

interface Texty a where
  toText : a -> String

data Tile
  = Wall
  | Empty
  | Player
  | Jackal

Texty Tile where
  toText Wall
    = "#"
  toText Empty
    = "."
  toText Player
    = "@"
  toText Jackal
    = "J"

Board : (rows : Nat) -> (cols : Nat) -> Type
Board rows cols = Matrix rows cols Tile

Texty (Vect cols Tile) where
  toText [] = ""
  toText (x :: xs) = toText x ++ toText xs

Texty (Board rows cols) where
  toText [] = ""
  toText (x :: xs) = toText x ++ "\n" ++ toText xs

mkRow : (cols : Nat) -> Vect cols Tile
mkRow Z = []
mkRow (S Z) = [Wall]
mkRow (S (S Z)) = [Wall, Wall]
mkRow (S (S (S k))) = let result = [Wall] ++ replicate (S k) Empty ++ [Wall] in
                          rewrite plusCommutative 1 k in result

wallsAround : Nat -> Nat
wallsAround k = S (S k)

mkBoard : (innerRows : Nat) -> (innerCols : Nat) -> (Board (S (S innerRows)) (S (S innerCols)))
mkBoard Z Z
  = [ [ Wall, Wall ]
    , [ Wall, Wall ]
    ]
mkBoard Z (S k) = replicate 2 (replicate (wallsAround (S k)) Wall)
mkBoard (S k) Z = replicate (S (S (S k))) [Wall, Wall]
mkBoard (S k) (S j)
  = let result =  [replicate _ Wall]
               ++ replicate (S k) (mkRow (wallsAround (S j)))
               ++ [replicate _ Wall] in
                  rewrite plusCommutative 1 k in result

main : IO ()
main = putStr $ toText (mkBoard 10 10)

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
