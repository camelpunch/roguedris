module Position

import Data.Vect

import Config

%default total
%access public export

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

