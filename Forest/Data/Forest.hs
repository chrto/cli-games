module Data.Forest where

data Forest a = FoundExit
              | Trail a (Forest a) (Forest a) (Forest a)

darkForest :: Forest Int
darkForest = Trail
  3
  (Trail
     2
     (Trail 1 FoundExit FoundExit FoundExit)
     (Trail
        2
        (Trail 1 FoundExit FoundExit FoundExit)
        (Trail
           2
           (Trail 1 FoundExit FoundExit FoundExit)
           (Trail
              2
              (Trail 1 FoundExit FoundExit FoundExit)
              (Trail 1 FoundExit FoundExit FoundExit)
              (Trail 1 FoundExit FoundExit FoundExit))
           (Trail 1 FoundExit FoundExit FoundExit))
        (Trail 1 FoundExit FoundExit FoundExit))
     (Trail 1 FoundExit FoundExit FoundExit))
  (Trail
     2
     (Trail 1 FoundExit FoundExit FoundExit)
     (Trail 1 FoundExit FoundExit FoundExit)
     (Trail 1 FoundExit FoundExit FoundExit))
  (Trail
     2
     (Trail 1 FoundExit FoundExit FoundExit)
     (Trail 1 FoundExit FoundExit FoundExit)
     (Trail 1 FoundExit FoundExit FoundExit))

deepForest ::Forest Int
deepForest = Trail
  3
  (Trail
     7
     (Trail 3 FoundExit FoundExit FoundExit)
     (Trail 4 FoundExit FoundExit FoundExit)
     (Trail 5 FoundExit FoundExit FoundExit))
  (Trail
     3
     (Trail 3 FoundExit FoundExit FoundExit)
     (Trail 9 FoundExit FoundExit FoundExit)
     (Trail 5 FoundExit FoundExit FoundExit))
  (Trail
     5
     (Trail 3 FoundExit FoundExit FoundExit)
     (Trail 4 FoundExit FoundExit FoundExit)
     (Trail 1 FoundExit FoundExit FoundExit))

availabelForests :: [(Int, String, Forest Int)]
availabelForests = zipWith (curry (\ (x, (y, z)) -> (x, y, z))) [1 :: Int .. ] [("darkForest", darkForest), ("deepForest", deepForest)]