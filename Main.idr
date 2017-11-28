module Main

import Data.Vect
import System

import Config
import Board
import Display
import Game
import NCurses
import Position

getValidKeyPress : IO (c ** ViMovement c)
getValidKeyPress = do
  chr <- getch
  (case isViMovement chr of
        (Yes prf) => pure (_ ** prf)
        (No contra) => getValidKeyPress)

renderLine : Vect Config.width Tile -> IO ()
renderLine line = do
  traverse_ (addch . symbol) line
  addch '\n'

showPlayerState : Character -> String
showPlayerState c
  = "Your health: " ++ show (hp c) ++ " Your coords: " ++ show (coords c) ++ " "

game : GameState -> IO Finished
game state = do
  mvaddstr (MkPoint 0 0) $ showPlayerState (player state)
  move (MkPoint 0 2)
  traverse_ renderLine (populate state)
  (c ** _) <- getValidKeyPress
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show c
  (case advance c state of
        next_turn@(MkGameState (MkCharacter Z _ _) _) => pure $ Lost next_turn
        next_turn@(MkGameState (MkCharacter (S k) _ _) _) => game next_turn)

newGame : GameState
newGame
  = MkGameState
    (MkCharacter 10 (MkPos 10 10) '@')
    [ (MkCharacter 10 (MkPos 5 5) 'J')
    ]

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game newGame
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
