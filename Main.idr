module Main

import Data.Vect
import System

import Board
import Config
import Display
import Game
import NCurses
import Position
import Random

getCommand : IO Command
getCommand = do
  chr <- getch
  mvaddstr (MkPoint 0 1) $ "You pressed " ++ show chr
  (case lookup chr keyMap of
        Nothing => getCommand
        (Just x) => pure x)

renderLine : Vect Config.width Tile -> IO ()
renderLine line = do
  traverse_ (addch . symbol) line
  addch '\n'

renderMob : Character -> IO ()
renderMob mob =
  addstr $ show (symbol mob) ++ ": " ++ show (hp mob) ++ " "

showPlayerState : Character -> String
showPlayerState c
  = "Your health: " ++ show (hp c) ++ " Your coords: " ++ show (coords c) ++ " "

game : GameState -> IO Finished
game state = do
  clear
  mvaddstr (MkPoint 0 0) $ showPlayerState (player state)
  move (MkPoint 0 2)
  traverse_ renderLine (populate state)
  addstr "Mobs:\n"
  traverse_ renderMob (mobs state)
  command <- getCommand
  let newState = advance command state
  case decEq (record { player->hp } newState) Z of
    Yes prf => pure $ Lost newState
    No contra => game newState

newGame : Stream (Fin 12) -> GameState
newGame nums
  = MkGameState
    ( MkCharacter 10 (MkPos 10 10) '@' nums )
    [ MkCharacter 10 (MkPos  5  5) 'J' nums
    , MkCharacter 10 (MkPos  7  7) 'S' nums
    ]

main : IO ()
main = do
  initscr
  timeout (-1)
  noecho
  curs_set 0
  result <- game (newGame (diceRolls (fromInteger !time)))
  mvaddstr (MkPoint 0 2) "You are dead."
  refresh
  usleep 1000000
  endwin
  exit 0

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
