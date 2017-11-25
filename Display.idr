import Data.Matrix

import Game

public export
Board : Type
Board = Matrix Game.height Game.width Char

emptyBoard : Board
emptyBoard = replicate _ (replicate _ '.')

replacePos : Position -> Char -> Board -> Board
replacePos (MkPos x y) c board
  = let oldRow = getRow y board
        newRow = replaceAt x c oldRow
    in  replaceAt y newRow board

drawCharacter : Character -> Board -> Board
drawCharacter (MkCharacter hp coords symbol) board = replacePos coords symbol board

export
populate : GameState -> Board
populate (MkGameState player _) = drawCharacter player emptyBoard

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
