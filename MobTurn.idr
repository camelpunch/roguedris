module MobTurn

import Data.Fin

import Position
import Character

data FightResult = MkFightResult Character Character

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

data SpatialRelationship : Character -> Character -> Type where
  Colocated : {auto prf : coords c1 = coords c2} ->
              SpatialRelationship c1 c2
  Apart     : {prf : coords c1 = coords c2 -> Void} ->
              SpatialRelationship c1 c2

public export
mobTurn : (playerStartPosition : Position) ->
          (Character, List Character) ->
          (mob : Character) ->
          (Character, List Character)
mobTurn playerStartPosition (player, mobs) mob
  = case spatialRelationship player mob of
         Colocated =>
           case fight player mob of
                MkFightResult p (MkCharacter Z _ _ _) =>
                  deadMob p
                MkFightResult p m@(MkCharacter (S k) _ _ _) =>
                  aliveMob p m
         Apart => (player, mobs ++ [mob])
    where
      spatialRelationship : (c1 : Character) ->
                            (c2 : Character) ->
                            SpatialRelationship c1 c2
      spatialRelationship c1 c2
        = case decEq (coords c1) (coords c2) of
               Yes _ => Colocated
               No contra => Apart {prf=contra}

      deadMob : (newPlayer : Character) -> (Character, List Character)
      deadMob p = (p, mobs)

      revertPlayerPosition : Character -> Character
      revertPlayerPosition = record { coords = playerStartPosition }

      aliveMob : (newPlayer : Character) ->
                 (newMob : Character) ->
                 (Character, List Character)
      aliveMob p m = (revertPlayerPosition p, mobs ++ [m])
