module GameState

import Data.Vect

import Character

public export
record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character

export
appendMob : (mob : Character) -> GameState -> GameState
appendMob mob = record { mobs $= (++ [mob]) }
