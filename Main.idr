module Main

import System
import Data.Vect

import NCurses
import Game

getValidKeyPress : IO (c ** ViMovement c)
getValidKeyPress = do
  chr <- getch
  (case isViMovement chr of
        (Yes prf) => pure (_ ** prf)
        (No contra) => getValidKeyPress)

game : PlayerState -> IO Finished
game state@(MkPlayerState hp coords) = do
  mvaddstr (MkPoint 0 0) $ "Your health: " ++ show hp ++ " Your coords: " ++ show coords ++ " "
  (c ** _) <- getValidKeyPress
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show c
  let state' = nextTurn c state
  case state' of
       (MkPlayerState Z coords) => pure $ Lost state'
       (MkPlayerState (S k) coords) => game state'

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game (MkPlayerState 10 (MkPos 10 10))
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0
