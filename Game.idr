module Game

import Data.Vect

import Character
import Config
import Position
import MobTurn

%default total

public export
record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character

public export
data Finished : Type where
  Lost : (state : GameState) ->
         { auto prf : record { player->hp } state = Z } ->
         Finished

public export
data Command = MoveLeft | MoveDown | MoveUp | MoveRight

public export
newGame : Stream (Fin 12) -> GameState
newGame nums
  = MkGameState
    ( MkCharacter 10 (MkPos 10 10) '@' nums )
    [ MkCharacter 10 (MkPos  5  5) 'J' nums
    , MkCharacter 10 (MkPos  7  7) 'S' nums
    ]

public export
keyMap : Vect 4 (Char, Command)
keyMap = [ ('h', MoveLeft)
         , ('j', MoveDown)
         , ('k', MoveUp)
         , ('l', MoveRight)
         ]

public export
advance : Command -> GameState -> GameState
advance command state
  = let newState             = playerMoveProposal command state
        (newPlayer, newMobs) = foldl (mobTurn (record { player->coords } state))
                                     (player newState, [])
                                     (mobs newState)
    in  MkGameState newPlayer (fromList newMobs)
    where
      playerMoveProposal : Command -> GameState -> GameState
      playerMoveProposal MoveLeft  = record { player->coords->x $= pred }
      playerMoveProposal MoveDown  = record { player->coords->y $= succ }
      playerMoveProposal MoveUp    = record { player->coords->y $= pred }
      playerMoveProposal MoveRight = record { player->coords->x $= succ }
