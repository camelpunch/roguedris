module Display

import Board
import Character

%access public export

interface Symbolic a where
  symbol : a -> Char

Symbolic Tile where
  symbol Empty = '.'
  symbol (Occupied c) = Character.symbol c

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
