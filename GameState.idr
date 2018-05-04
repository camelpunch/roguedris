module GameState

import Data.Vect

import Character

public export
record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character