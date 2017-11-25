module Main

import System
import Data.Vect

import NCurses
import Game
import Display

getValidKeyPress : IO (c ** ViMovement c)
getValidKeyPress = do
  chr <- getch
  (case isViMovement chr of
        (Yes prf) => pure (_ ** prf)
        (No contra) => getValidKeyPress)

renderLine : Vect Game.width Char -> IO ()
renderLine line = do
  traverse_ addch line
  addch '\n'

game : GameState -> IO Finished
game state@(MkGameState (MkPlayerState hp coords)) = do
  mvaddstr (MkPoint 0 0) $ "Your health: " ++ show hp ++ " Your coords: " ++ show coords ++ " "
  move (MkPoint 0 2)
  traverse_ renderLine (populate state)
  (c ** _) <- getValidKeyPress
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show c
  let state' = nextTurn c state
  (case state' of
        (MkGameState (MkPlayerState Z coords)) => pure $ Lost state'
        (MkGameState (MkPlayerState (S k) coords)) => game state')

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game (MkGameState $ MkPlayerState 10 (MkPos 10 10))
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
