{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module User.Actions.Attack where

import System.Random  (randomRIO)
import Text.Read (readMaybe)

data Battle = Fight | RunAway
  deriving (Show, Read, Eq)

data CreatureType = Golem | Spider | Dragon | Player
  deriving (Show)

data Creature = Creature
  {creature:: CreatureType
  , healt :: Int
  , power :: Int
  }

spider :: IO Creature
spider = do
  healt <- randomRIO @Int (5, 20)
  power  <- randomRIO @Int (1, 3)
  return Creature {creature = Spider, ..} --RecordWildCards
  -- return Creature {creature = Spider, healt, power} --NamedFieldPuns

golem :: IO Creature
golem = do

  healt <- randomRIO @Int (10, 50)
  power  <- randomRIO @Int (7, 13)
  return Creature {creature = Golem, ..} --RecordWildCards
  -- return Creature {creature = Golem, power, healt} --NamedFieldPuns

dragon :: IO Creature
dragon = do
  healt <- randomRIO @Int (10, 50)
  power  <- randomRIO @Int (8, 18)
  return Creature {creature = Dragon, ..} --RecordWildCards
  -- return Creature {creature = Dragon, healt, power} --NamedFieldPuns

getAttacker :: IO Creature
getAttacker = do
  r <- randomRIO @Int (1, 3)
  case r of
    3 -> dragon
    2 -> golem
    _ -> spider

player :: IO Creature
player = do
  healt <- randomRIO @Int (50, 70)
  power <- randomRIO @Int (6, 10)
  return Creature {creature = Player, ..} --RecordWildCards
  -- return Creature {creature = Player, healt, power} --NamedFieldPuns

getAttackPower :: Creature -> IO Int
getAttackPower c = (* power c) <$> randomRIO @Int (1, 3)

setDamage :: Creature -> Int -> Creature
setDamage c@Creature{..} p | healt > p = c { healt = healt - p } --RecordWildCards
-- setDamage c@Creature{healt} p | healt > p = c { healt = healt - p } --NamedFieldPuns
setDamage c _                           = c { healt = 0 }

fight :: (Creature, Creature) -> IO (Creature, Creature)
fight (p, m) = do
  m' <- setDamage m <$> getAttackPower p
  p' <- setDamage p <$> getAttackPower m
  return (p', m')

escape :: (Creature, Creature) -> IO (Creature, Creature)
escape (p, m) = do
  r <- randomRIO @Int (1, 3)
  case r of
    2 -> do return (p, m)
    _ -> do
      p' <- setDamage p <$> getAttackPower m
      return (p', m)

startBattle :: Creature -> Creature -> IO Bool
startBattle p m = do
  putStrLn $ "The " ++ show (creature m) ++ " has " ++ show (healt m) ++ " healt and " ++ show (power m) ++ " attack power."
  putStrLn $ "You have " ++ show (healt p) ++ " healt and " ++ show (power p) ++ " attack power."
  putStrLn "Choose 'Fight' to fight or 'RunAway' to run away."
  selectedBattle <- readMaybe @Battle <$> getLine
  battleResult <- case selectedBattle of
    Just Fight -> do fight (p, m)
    Just RunAway -> do escape (p, m)
    Nothing -> do putStrLn "Invalid action. Choose 'Fight' or 'RunAway'." >> return (p, m)

  case battleResult of
    (_, Creature{healt = 0}) -> putStrLn "You Won!" >>  return True
    (Creature{healt = 0}, _) -> putStrLn "You died!" >> return False
    (Creature{healt = h}, _)
      | h == healt p && selectedBattle == Just RunAway  -> putStrLn "You escaped!" >> return True
    (p', m') -> startBattle p' m' -- in case of wrong battle action

battle :: IO Bool
battle = do
  m <- getAttacker
  p <- player
  putStrLn $ "You've encountered a " ++ show (creature m) ++ "! Choose an action: 'Fight' or 'RunAway'?"
  startBattle p m
