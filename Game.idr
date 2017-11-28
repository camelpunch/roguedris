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

xCoordsDiffer : {x : GameX} ->
                (contra : (x = x') -> Void) ->
                (MkPos x _ = MkPos x' _) ->
                Void
xCoordsDiffer contra Refl = contra Refl

yCoordsDiffer : {x : GameX} ->
                {y : GameY} ->
                (prf : x = x') ->
                (contra : (y = y') -> Void) ->
                (MkPos x y = MkPos x' y') ->
                Void
yCoordsDiffer Refl contra Refl = contra Refl

xAndYCoordsMatch : {x : GameX} ->
                   {y : GameY} ->
                   (prfX : x = x') ->
                   (prfY : y = y') ->
                   Dec (MkPos x y = MkPos x' y')
xAndYCoordsMatch Refl Refl = Yes Refl

DecEq Position where
  decEq (MkPos x y) (MkPos x' y')
    = case decEq x x' of
           (Yes prfX) => (case decEq y y' of
                              (Yes prfY) => xAndYCoordsMatch prfX prfY
                              (No contra) => No (yCoordsDiffer prfX contra))
           (No contra) => No (xCoordsDiffer contra)

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

IsViMovement : Char -> Type
IsViMovement = Dec . ViMovement

isViMovement : (c : Char) -> IsViMovement c
isViMovement c = isElem c validViMovements

char2Movement : (c : Char) -> { auto prf : IsViMovement c } -> Movement
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
          { auto prf : IsViMovement c } ->
          GameState
advance c state
  = let candidate = coords (move (char2Movement c) (player state))
    in case isElem candidate (map coords (mobs state)) of
         (Yes prf) => state
         (No contra) => record { player $= move (char2Movement c) } state
