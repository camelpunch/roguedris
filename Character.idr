module Character

import Data.Fin

import Position

public export
record Character where
  constructor MkCharacter
  hp : Nat
  coords : Position
  symbol : Char
  attackPoints : Stream (Fin 12)
