module MobTurn

import Data.Vect

import Character
import GameState
import Position

%default total

data FightResult = MkFightResult Character Character

data SpatialRelationship : Character -> Character -> Type where
  Colocated : {auto prf : coords c1 = coords c2} ->
              SpatialRelationship c1 c2
  Apart     : {prf : coords c1 = coords c2 -> Void} ->
              SpatialRelationship c1 c2

fight : (c1 : Character) ->
        (c2 : Character) ->
        {auto prf : coords c1 = coords c2} ->
        FightResult
fight c1 c2
  = case attackPoints c1 of
         (damage :: futurePoints) =>
           MkFightResult
             (record { attackPoints = futurePoints } c1)
             (record { hp $= (`minus` (finToNat damage)) } c2)

export
mobTurn : (playerStartPosition : Position) ->
          GameState ->
          (mob : Character) ->
          GameState
mobTurn playerStartPosition state mob
  = case spatialRelationship (player state) mob of
         Colocated => attack (player state) mob
         Apart => appendMob mob state
    where
      attack : (attacker : Character) ->
               (defender : Character) ->
               {auto prf : coords attacker = coords defender} ->
               GameState
      attack attacker defender =
        case fight attacker defender of
             MkFightResult p (MkCharacter Z _ _ _) => state
             MkFightResult p m@(MkCharacter (S k) _ _ _) => aliveMob m
        where
          aliveMob : (mob : Character) -> GameState
          aliveMob m = record { player->coords = playerStartPosition }
                              (appendMob m state)

      spatialRelationship : (c1 : Character) ->
                            (c2 : Character) ->
                            SpatialRelationship c1 c2
      spatialRelationship c1 c2
        = case decEq (coords c1) (coords c2) of
               Yes _ => Colocated
               No contra => Apart {prf=contra}
