module User.Actions.Move where

import Forest.Forests    (Forest (..))

data AvailableMoves = GoLeft
          | GoRight
          | GoForward
  deriving (Show, Read)

move :: Num a => (a, Forest a) -> AvailableMoves -> (a, Forest a)
move (s, FoundExit) _ = (s, FoundExit)
move (s, Trail n m _ _) GoLeft = (s - n, m)
move (s, Trail n _ m _) GoForward = (s - n, m)
move (s, Trail n _ _ m) GoRight = (s - n, m)
