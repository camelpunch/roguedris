module Game

public export
data PlayerState : (hp : Nat) -> Type where
     MkPlayerState : (new_hp : Nat) ->
                     PlayerState new_hp

public export
data Finished : Type where
  Lost : (game : PlayerState Z) -> Finished

