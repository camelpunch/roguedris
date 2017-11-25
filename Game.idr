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
  show n = show $ finToNat n

record Position where
  constructor MkPos
  x : GameX
  y : GameY

Show Position where
  show (MkPos x y) = show x ++ "x" ++ show y

record PlayerState where
  constructor MkPlayerState
  hp : Nat
  coords : Position

Show PlayerState where
  show (MkPlayerState hp coords)
    = "HP: " ++ show hp ++ " -- coords: " ++ show coords

data Finished : Type where
  Lost : (game : PlayerState) -> Finished

data Movement = L | D | U | R

validViMovements : Vect 4 Char
validViMovements = ['h', 'j', 'k', 'l']

ViMovement : (c : Char) -> Type
ViMovement c = Elem c validViMovements

IsViMovement : (c : Char) -> Type
IsViMovement c = Dec (ViMovement c)

isViMovement : (c : Char) -> Dec (ViMovement c)
isViMovement c = isElem c validViMovements

fromChar : (c : Char) -> { auto prf : IsViMovement c } -> Movement
fromChar 'h' = L
fromChar 'j' = D
fromChar 'k' = U
fromChar _   = R

move : Movement -> PlayerState -> PlayerState
move L = record { coords->x $= pred }
move D = record { coords->y $= succ }
move U = record { coords->y $= pred }
move R = record { coords->x $= succ }

nextTurn : (c : Char) ->
           (ps : PlayerState) ->
           { auto prf : IsViMovement c } ->
           PlayerState
nextTurn c ps@(MkPlayerState Z coords) = ps
nextTurn c ps@(MkPlayerState (S k) coords) = move (fromChar c) ps
