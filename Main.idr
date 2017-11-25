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

showPlayerState : Character -> String
showPlayerState ps
  = "Your health: " ++ show (hp ps) ++ " Your coords: " ++ show (coords ps) ++ " "

game : GameState -> IO Finished
game state = do
  mvaddstr (MkPoint 0 0) $ showPlayerState (player state)
  move (MkPoint 0 2)
  traverse_ renderLine (populate state)
  (c ** _) <- getValidKeyPress
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show c
  (case advance c state of
        next_turn@(MkGameState (MkCharacter Z _) _) => pure $ Lost next_turn
        next_turn@(MkGameState (MkCharacter (S k) _) _) => game next_turn)

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game (MkGameState (MkCharacter 10 (MkPos 10 10)) [])
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
