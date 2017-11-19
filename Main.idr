module Main

import System

import NCurses
import Game

nextTurn : Char -> PlayerState -> PlayerState
nextTurn c state@(MkPlayerState Z) = state
nextTurn c (MkPlayerState (S k)) = MkPlayerState k

game : PlayerState -> IO Finished
game state@(MkPlayerState hp) = do
  mvaddstr (MkPoint 0 0) $ "Your health: " ++ show hp ++ " "
  c <- getch
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show c
  let state' = nextTurn c state
  case state' of
       (MkPlayerState Z) => pure $ Lost state'
       (MkPlayerState (S k)) => game state'

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game (MkPlayerState 10)
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0
