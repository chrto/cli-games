
data Move = GoLeft
          | GoRight
          | GoForward
  deriving (Show, Eq)

data Maze = Wall
          | Exit
          | Inside Maze Maze Maze
  deriving (Show)

testMaze = Inside
  (Inside Wall Wall Wall)
  (Inside Wall (Inside Wall Exit Wall) Wall)
  (Inside Wall Wall Wall) :: Maze

move :: Maze -> Move -> Maze
move Wall _ = Wall
move Exit _ = Exit
move (Inside l f r) m
  | m == GoLeft = l
  | m == GoRight = r
  | m == GoForward = f
  | otherwise = Inside l f r

solveMaze' :: Maze -> [Move] -> Maze
solveMaze' Wall _ = Wall
solveMaze' Exit _ = Exit
solveMaze' m [] = m
solveMaze' maze (m:ms) = solveMaze' (move maze m) ms

solveMaze'' :: Maze -> [Move] -> Maze
solveMaze'' = foldl move

solveMaze :: Maze -> [Move] -> String
solveMaze maze = showCurrentChoice . solveMaze' maze

showCurrentChoice :: Maze -> String
showCurrentChoice Wall = "You've hit a wall!"
showCurrentChoice Exit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice Inside {} =
  "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

main :: IO ()
main = do
  putStrLn $ solveMaze testMaze [GoForward, GoForward, GoForward]
  -- putStrLn $ solveMaze testMaze [GoForward]
  -- putStrLn $ solveMaze testMaze [GoForward, GoForward, GoRight]