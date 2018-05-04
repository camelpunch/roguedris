module Game

import Data.Vect

import Config
import Position

%default total

public export
record Character where
  constructor MkCharacter
  hp : Nat
  coords : Position
  symbol : Char
  attackPoints : Stream (Fin 12)

public export
record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character

public export
data Finished : Type where
  Lost : (state : GameState) ->
         { auto prf : record { player->hp } state = Z } ->
         Finished

public export
data Command = MoveLeft | MoveDown | MoveUp | MoveRight

public export
newGame : Stream (Fin 12) -> GameState
newGame nums
  = MkGameState
    ( MkCharacter 10 (MkPos 10 10) '@' nums )
    [ MkCharacter 10 (MkPos  5  5) 'J' nums
    , MkCharacter 10 (MkPos  7  7) 'S' nums
    ]

public export
keyMap : Vect 4 (Char, Command)
keyMap = [ ('h', MoveLeft)
         , ('j', MoveDown)
         , ('k', MoveUp)
         , ('l', MoveRight)
         ]

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

spatialRelationship : (c1 : Character) ->
                      (c2 : Character) ->
                      SpatialRelationship c1 c2
spatialRelationship c1 c2
  = case decEq (coords c1) (coords c2) of
         Yes _ => Colocated
         No contra => Apart {prf=contra}

public export
advance : Command -> GameState -> GameState
advance command state
  = let newState             = playerMoveProposal command state
        (newPlayer, newMobs) = foldl (mobTurn (record { player->coords } state))
                                     (player newState, [])
                                     (mobs newState)
    in  MkGameState newPlayer (fromList newMobs)
    where
      playerMoveProposal : Command -> GameState -> GameState
      playerMoveProposal MoveLeft  = record { player->coords->x $= pred }
      playerMoveProposal MoveDown  = record { player->coords->y $= succ }
      playerMoveProposal MoveUp    = record { player->coords->y $= pred }
      playerMoveProposal MoveRight = record { player->coords->x $= succ }

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
            deadMob : (newPlayer : Character) -> (Character, List Character)
            deadMob p = (p, mobs)

            revertPlayerPosition : Character -> Character
            revertPlayerPosition = record { coords = playerStartPosition }

            aliveMob : (newPlayer : Character) ->
                       (newMob : Character) ->
                       (Character, List Character)
            aliveMob p m = (revertPlayerPosition p, mobs ++ [m])
