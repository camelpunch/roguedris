import Data.Matrix

import Game

public export
Board : Type
Board = Matrix Game.height Game.width Char

emptyBoard : Board
emptyBoard = replicate _ $ replicate _ '.'

export
populate : PlayerState -> Board
populate (MkPlayerState hp (MkPos x y))
  = let oldRow = getRow y emptyBoard
        newRow = replaceAt x '@' oldRow
    in  replaceAt y newRow emptyBoard

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
