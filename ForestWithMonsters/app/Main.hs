module Main where

import            Forest.Forests (Forest(..), availabelForests)
import            User.Actions.Move (AvailableMoves(..), move)

import            Data.Char (digitToInt, isDigit)
import            System.Random (randomRIO)
import            User.Actions.Attack (battle)

getUserChoiceDigit :: IO Int
getUserChoiceDigit = getUserChoiceChar
  >>= \c -> if isDigit c
            then return (digitToInt c)
            else return 0

getUserChoiceChar :: IO Char
getUserChoiceChar = getChar <* putStrLn ""

getUserChoiceStr :: IO String
getUserChoiceStr = getLine

buildString :: [String] -> String
buildString [] = ""
buildString (x:xs) = show x ++ " " ++ buildString xs

finishGame :: IO ()
finishGame = putStrLn "Goodbye!"

playOrExit :: IO ()
playOrExit = putStrLn
  "Do you want to play again? Hit 'y' to play again or 'n' to exit."
  >> getUserChoiceChar
  >>= \choice -> if choice == 'y'
                 then startGame
                 else finishGame

playGame :: (Num a, Ord a, Show a) => (a, Forest a) -> IO ()
playGame (_, FoundExit) = putStrLn "YOU'VE FOUND THE EXIT!!" >> playOrExit
playGame (s, _)
  | s <= 0 = putStrLn "You ran out of stamina and died -.-!" >> playOrExit
playGame (s, forest) = do
  r <- randomRIO @Int (1, 3)
  case r of
    2 -> monsterInForest (s, forest)
    _ -> emptyForest (s, forest)

monsterInForest :: (Num a, Ord a, Show a) => (a, Forest a) -> IO ()
monsterInForest(s, forest) = do
  r <- battle
  if r then emptyForest (s, forest)
       else playOrExit

emptyForest :: (Num a, Ord a, Show a) => (a, Forest a) -> IO ()
emptyForest (s, forest) = putStrLn
  (buildString
     [ "You are in a forest with "
     , show s
     , " stamina left. Choose a direction: GoLeft, GoRight or GoForward."])
  >> getUserChoiceStr
  >>= \choice
  -> let validChoices = choice `elem` map show [GoLeft, GoRight, GoForward]
     in if not validChoices
        then putStrLn "\nInvalid choice, try again." >> playGame (s, forest)
        else playGame . move (s, forest) . read @AvailableMoves $ choice

choseForest :: IO (Forest Int)
choseForest = do
  putStrLn
    $ "Choose a forest to play:"
    ++ foldl
      (\acc (i, name, _) -> acc ++ "\n" ++ show i ++ ". " ++ name)
      ""
      availabelForests
  getForest availabelForests
  where
    getForest :: [(Int, String, Forest Int)] -> IO (Forest Int)
    getForest fs = getUserChoiceDigit
      >>= (\forest
           -> if null forest
              then putStrLn "Invalid choice, try again." >> getForest fs
              else return . (\(_, _, f) -> f) . head $ forest)
      . (\choice -> filter (\(i, _, _) -> i == choice) fs)

startGame :: IO ()
startGame = do
  startingStamina <- randomRIO @Int (10_000, 20_000)
  forest <- choseForest
  playGame (startingStamina, forest)
-- startGame = choseForest >>= playGame . (,) 10

main :: IO ()
main = putStrLn
  "You are trapped in a forest, try to escape! Remember that you loose stamina with each move. Good luck!"
  >> putStrLn
    "You can move left, right or forward. Type 'GoLeft' for left, 'GoRight' for right and 'GoForward' for forward."
  >> startGame
