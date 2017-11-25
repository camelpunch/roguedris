module Game

import Data.Vect

%default total
%access public export

width : Nat
width = 20

height : Nat
height = 15

GameX : Type
GameX = Fin width

GameY : Type
GameY = Fin height

Show (Fin a) where
  show = show . finToNat

record Position where
  constructor MkPos
  x : GameX
  y : GameY

Show Position where
  show (MkPos x y) = show x ++ "x" ++ show y

record Character where
  constructor MkCharacter
  hp : Nat
  coords : Position

record GameState where
  constructor MkGameState
  player : Character

data Finished : Type where
  Lost : (game : GameState) -> Finished

data Movement = L | D | U | R

validViMovements : Vect 4 Char
validViMovements = ['h', 'j', 'k', 'l']

ViMovement : (c : Char) -> Type
ViMovement c = Elem c validViMovements

IsViMovement : Char -> Type
IsViMovement = Dec . ViMovement

isViMovement : (c : Char) -> IsViMovement c
isViMovement c = isElem c validViMovements

fromChar : (c : Char) -> { auto prf : IsViMovement c } -> Movement
fromChar 'h' = L
fromChar 'j' = D
fromChar 'k' = U
fromChar _   = R

move : Movement -> Character -> Character
move L = record { coords->x $= pred }
move D = record { coords->y $= succ }
move U = record { coords->y $= pred }
move R = record { coords->x $= succ }

nextTurn : (c : Char) ->
           (gs : GameState) ->
           { auto prf : IsViMovement c } ->
           GameState
nextTurn c gs@(MkGameState (MkCharacter Z _)) = gs
nextTurn c (MkGameState ps@(MkCharacter (S k) _)) = MkGameState (move (fromChar c) ps)
