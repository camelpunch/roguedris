module MobTurn

import Data.Vect

import Character
import GameState
import Position

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

public export
mobTurn : (playerStartPosition : Position) ->
          GameState ->
          (mob : Character) ->
          GameState
mobTurn playerStartPosition gameChars mob
  = case spatialRelationship (player gameChars) mob of
         Colocated => attack (player gameChars) mob
         Apart => MkGameState (player gameChars)
                              (mobs gameChars ++ [mob])
    where
      attack : (attacker : Character) ->
               (defender : Character) ->
               {auto prf : coords attacker = coords defender} ->
               GameState
      attack attacker defender =
        case fight attacker defender of
             MkFightResult p (MkCharacter Z _ _ _) =>
               deadMob p
             MkFightResult p m@(MkCharacter (S k) _ _ _) =>
               aliveMob p m
        where
          deadMob : (newPlayer : Character) -> GameState
          deadMob p = MkGameState p (mobs gameChars)

          revertPlayerPosition : Character -> Character
          revertPlayerPosition = record { coords = playerStartPosition }

          aliveMob : (newPlayer : Character) ->
                     (newMob : Character) ->
                     GameState
          aliveMob p m = MkGameState (revertPlayerPosition p)
                                     (mobs gameChars ++ [m])

      spatialRelationship : (c1 : Character) ->
                            (c2 : Character) ->
                            SpatialRelationship c1 c2
      spatialRelationship c1 c2
        = case decEq (coords c1) (coords c2) of
               Yes _ => Colocated
               No contra => Apart {prf=contra}
