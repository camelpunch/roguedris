module Game

import Data.Vect

import Config
import Position

%default total
%access public export

record Character where
  constructor MkCharacter
  hp : Nat
  coords : Position
  symbol : Char

record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character

data Finished : Type where
  Lost : (state : GameState) ->
         { auto prf : record { player->hp } state = Z } ->
         Finished

data Command = MoveLeft | MoveDown | MoveUp | MoveRight

keyMap : Vect 4 (Char, Command)
keyMap = [ ('h', MoveLeft)
         , ('j', MoveDown)
         , ('k', MoveUp)
         , ('l', MoveRight)
         ]

process : Command -> GameState -> GameState
process MoveLeft  = record { player->coords->x $= pred }
process MoveDown  = record { player->coords->y $= succ }
process MoveUp    = record { player->coords->y $= pred }
process MoveRight = record { player->coords->x $= succ }

advance : Command -> GameState -> GameState
advance command state
  = let state' = process command state
        obstacles = coords <$> mobs state
    in case isElem (record { player->coords } state') obstacles of
       (Yes prf) => state
       (No contra) => state'
