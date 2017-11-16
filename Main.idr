module Main

import System

import ForeignFunctions
import Game

showHealth : Nat -> IO ()
showHealth hp = do
  mvaddstr (MkPoint 0 1) $ "Your health: " ++ show hp
  pure ()

game : PlayerState (S hp) -> IO Finished
game {hp} state = do
  c <- getch
  mvaddstr (MkPoint 0 0) $ "You pressed " ++ show c
  showHealth hp
  (case hp of
        Z => pure $ Lost (MkPlayerState Z)
        (S k) => game (MkPlayerState (S k)))

startState : PlayerState hp
startState = MkPlayerState hp

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
