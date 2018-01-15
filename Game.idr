module Game

import Data.Vect

import Config
import Position

%default total
%access public export

record Character where
  constructor MkCharacter
  hp : Nat
  coords : Position
  symbol : Char
  attackPoints : Stream (Fin 12)

record GameState where
  constructor MkGameState
  player : Character
  mobs : Vect n Character

data Finished : Type where
  Lost : (state : GameState) ->
         { auto prf : record { player->hp } state = Z } ->
         Finished

data Command = MoveLeft | MoveDown | MoveUp | MoveRight

keyMap : Vect 4 (Char, Command)
keyMap = [ ('h', MoveLeft)
         , ('j', MoveDown)
         , ('k', MoveUp)
         , ('l', MoveRight)
         ]

playerMoveProposal : Command -> GameState -> GameState
playerMoveProposal MoveLeft  = record { player->coords->x $= pred }
playerMoveProposal MoveDown  = record { player->coords->y $= succ }
playerMoveProposal MoveUp    = record { player->coords->y $= pred }
playerMoveProposal MoveRight = record { player->coords->x $= succ }

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
  Distant : {prf : coords c1 = coords c2 -> Void} ->
            SpatialRelationship c1 c2

spatialRelationship : (c1 : Character) ->
                      (c2 : Character) ->
                      SpatialRelationship c1 c2
spatialRelationship c1 c2
  = case decEq (coords c1) (coords c2) of
         Yes _ => Colocated
         No contra => Distant {prf=contra}

processMob : (startPosition : Position) ->
             (processed : (Character, List Character)) ->
             (mob : Character) ->
             (Character, List Character)
processMob startPosition (player, mobs) mob
  = case spatialRelationship player mob of
         Colocated =>
           case fight player mob of
                MkFightResult newPlayer (MkCharacter Z _ _ _) =>
                  ( newPlayer
                  , mobs
                  )
                MkFightResult newPlayer stillAliveMob@(MkCharacter (S k) _ _ _) =>
                  ( record { coords = startPosition } newPlayer
                  , mobs ++ [stillAliveMob]
                  )
         Distant =>
           ( player
           , mobs ++ [mob]
           )

advance : Command -> GameState -> GameState
advance command state
  = let newState             = playerMoveProposal command state
        (newPlayer, newMobs) = foldl (processMob (record { player->coords } state))
                                     (player newState, [])
                                     (mobs newState)
    in  MkGameState newPlayer (fromList newMobs)
