
module Forest.Forests where

data Forest a = FoundExit
              | Trail a (Forest a) (Forest a) (Forest a)

darkForest :: Forest Int
darkForest = Trail
  3_000
  (Trail
     2_000
     (Trail 1_000 FoundExit FoundExit FoundExit)
     (Trail
        2_000
        (Trail 1_000 FoundExit FoundExit FoundExit)
        (Trail
           2_000
           (Trail 1_000 FoundExit FoundExit FoundExit)
           (Trail
              2_000
              (Trail 10_000 FoundExit FoundExit FoundExit)
              (Trail 1_000 FoundExit FoundExit FoundExit)
              (Trail 1_000 FoundExit FoundExit FoundExit))
           (Trail 1_000 FoundExit FoundExit FoundExit))
        (Trail 10_000 FoundExit FoundExit FoundExit))
     (Trail 1_000 FoundExit FoundExit FoundExit))
  (Trail
     2_000
     (Trail 1_000 FoundExit FoundExit FoundExit)
     (Trail 1_000 FoundExit FoundExit FoundExit)
     (Trail 1_000 FoundExit FoundExit FoundExit))
  (Trail
     2_000
     (Trail 1_000 FoundExit FoundExit FoundExit)
     (Trail 10_000 FoundExit FoundExit FoundExit)
     (Trail 1_000 FoundExit FoundExit FoundExit))

deepForest ::Forest Int
deepForest = Trail
  3_000
  (Trail
     7_000
     (Trail 3_000 FoundExit FoundExit FoundExit)
     (Trail 4_000 FoundExit FoundExit FoundExit)
     (Trail 5_000 FoundExit FoundExit FoundExit))
  (Trail
     3_000
     (Trail 3_000 FoundExit FoundExit FoundExit)
     (Trail 9_000 FoundExit FoundExit FoundExit)
     (Trail 5_000 FoundExit FoundExit FoundExit))
  (Trail
     5_000
     (Trail 3_000 FoundExit FoundExit FoundExit)
     (Trail 4_000 FoundExit FoundExit FoundExit)
     (Trail 1_000 FoundExit FoundExit FoundExit))

availabelForests :: [(Int, String, Forest Int)]
availabelForests = zipWith (curry  (\ (x, (y, z)) -> (x, y, z))) [1 .. ] [("darkForest", darkForest), ("deepForest", deepForest)] -- TODO