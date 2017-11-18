module Game

import Data.Vect

%default total
%access public export

data PlayerState : (hp : Nat) -> Type where
     MkPlayerState : (new_hp : Nat) ->
                     PlayerState new_hp

data Finished : Type where
  Lost : (game : PlayerState Z) -> Finished

data Movement = L | R | U | Still

data Position : (x : Nat) -> (y : Nat) -> Type where
     MkPos : (x : Nat) -> (y : Nat) -> Position x y

data ValidMovement : Movement -> Position x y -> Type where
     LeftAvailable : (Position (S x) y) -> ValidMovement L (MkPos (S x) y)
     RightAvailable : (Position x y) -> ValidMovement R (MkPos x y)
     UpAvailable   : (Position x (S y)) -> ValidMovement U (MkPos x (S y))

char2KeyPress : Char -> Movement
char2KeyPress 'h' = L
-- char2KeyPress 'j' = Just Down
char2KeyPress 'k' = U
-- char2KeyPress 'l' = Just Right
char2KeyPress _   = Still

cannotGoLeft : ValidMovement L (MkPos 0 _) -> Void
cannotGoLeft (LeftAvailable _) impossible

cannotGoUp : ValidMovement U (MkPos _ 0) -> Void
cannotGoUp (UpAvailable _) impossible

isValidMovement : (pos : Position x y) ->
                  (movement : Movement) ->
                  Dec (ValidMovement movement pos)
isValidMovement (MkPos Z _) L = No cannotGoLeft
isValidMovement pos@(MkPos (S x) _) L = Yes (LeftAvailable pos)
isValidMovement (MkPos _ Z) U = No cannotGoUp
isValidMovement pos@(MkPos _ (S k)) U = Yes (UpAvailable pos)


