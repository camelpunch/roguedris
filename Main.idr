import Data.Matrix

data Tile = Wall | Space | Occupied

interface Console a where
  consoleStr : a -> String

interface ConsoleChar a where
  consoleChar : a -> Char

ConsoleChar Tile where
  consoleChar Wall = '#'
  consoleChar Space = '.'
  consoleChar Occupied = '@'

Board : Nat -> Nat -> Type
Board rows cols = Matrix rows cols Tile

Console (Board rows cols) where
  consoleStr [] = ""
  consoleStr (row :: rows) = pack (map consoleChar row) ++ "\n" ++ consoleStr rows

horzWall : Vect n Tile
horzWall {n} = replicate n Wall

myboard : Board 3 5
myboard =
  [horzWall] ++
  [[Wall, Space, Space, Occupied, Wall]] ++
  [horzWall]

main : IO ()
main = putStrLn $ consoleStr myboard

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
