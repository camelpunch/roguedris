module Board

import Data.Matrix

import Character
import Config
import Game
import Position

public export
data Tile : Type where
  Empty : Tile
  Occupied : Character -> Tile

public export
Board : Type
Board = Matrix height width Tile

emptyBoard : Board
emptyBoard = replicate _ (replicate _ Empty)

replacePos : Position -> Tile -> Board -> Board
replacePos (MkPos x y) tile board
  = let oldRow = getRow y board
        newRow = replaceAt x tile oldRow
    in  replaceAt y newRow board

drawCharacter : Character -> Board -> Board
drawCharacter c board = replacePos (coords c) (Occupied c) board

export
populate : GameState -> Board
populate state
  = foldr drawCharacter emptyBoard (player state :: mobs state)

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
