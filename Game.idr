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
  Lost : (game : GameState) -> Finished

data Movement = L | D | U | R

validViMovements : Vect 4 Char
validViMovements = ['h', 'j', 'k', 'l']

ViMovement : (c : Char) -> Type
ViMovement c = Elem c validViMovements

isViMovement : (c : Char) -> Dec (ViMovement c)
isViMovement c = isElem c validViMovements

char2Movement : (c : Char) -> { auto prf : Dec (ViMovement c) } -> Movement
char2Movement 'h' = L
char2Movement 'j' = D
char2Movement 'k' = U
char2Movement _   = R

move : Movement -> Character -> Character
move L = record { coords->x $= pred }
move D = record { coords->y $= succ }
move U = record { coords->y $= pred }
move R = record { coords->x $= succ }

advance : (c : Char) ->
          (gs : GameState) ->
          { auto prf : Dec (ViMovement c) } ->
          GameState
advance c state
  = let candidate = coords (move (char2Movement c) (player state))
    in case isElem candidate (map coords (mobs state)) of
         (Yes prf) => state
         (No contra) => record { player $= move (char2Movement c) } state
