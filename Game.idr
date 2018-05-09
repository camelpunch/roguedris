module Game

import Data.Vect

import Character
import Config
import GameState
import Position
import MobTurn

%default total

public export
data Command
  = MoveLeft
  | MoveDown
  | MoveUp
  | MoveRight
  | Quit

public export
newGame : Stream (Fin 12) -> GameState
newGame nums
  = MkGameState
    ( MkCharacter 10 (MkPos 10 10) '@' nums )
    [ MkCharacter 10 (MkPos  5  5) 'J' nums
    , MkCharacter 10 (MkPos  7  7) 'S' nums
    ]

public export
advance : Command -> GameState -> GameState
advance command oldState
  = let newState = playerMoveProposal command oldState
    in  foldl (mobTurn (record { player->coords } oldState))
              (MkGameState (player newState) [])
              (mobs newState)
    where
      playerMoveProposal : Command -> GameState -> GameState
      playerMoveProposal MoveLeft  = record { player->coords->x $= pred }
      playerMoveProposal MoveDown  = record { player->coords->y $= succ }
      playerMoveProposal MoveUp    = record { player->coords->y $= pred }
      playerMoveProposal MoveRight = record { player->coords->x $= succ }
      playerMoveProposal Quit      = record { player->hp = Z }
