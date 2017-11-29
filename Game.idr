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

process : Command -> GameState -> GameState
process MoveLeft  = record { player->coords->x $= pred }
process MoveDown  = record { player->coords->y $= succ }
process MoveUp    = record { player->coords->y $= pred }
process MoveRight = record { player->coords->x $= succ }

data FightResult = MkFightResult Character Character

fight : (a : Character) ->
        (b : Character) ->
        { auto prf : coords a = coords b } ->
        FightResult
fight a b = MkFightResult a (record { hp $= pred } b)

processMob : (origCoords : Position) ->
             (processed : (Character, List Character)) ->
             (mob : Character) ->
             (Character, List Character)
processMob origCoords (player, retainedMobs) mob
  = case decEq (coords player) (coords mob) of
         Yes prf => case fight player mob of
                         MkFightResult newPlayer (MkCharacter Z _ _) =>
                           (newPlayer, retainedMobs)
                         MkFightResult newPlayer newMob@(MkCharacter (S k) _ _) =>
                           (record { coords = origCoords } newPlayer, retainedMobs ++ [newMob])
         No contra => (player, retainedMobs ++ [mob])

advance : Command -> GameState -> GameState
advance command state
  = let newState             = process command state
        (newPlayer, newMobs) = foldl (processMob (record { player->coords } state))
                                     (player newState, [])
                                     (mobs newState)
    in  MkGameState newPlayer (fromList newMobs)
