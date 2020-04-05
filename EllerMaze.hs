module Main where

import           Data.List
import           System.Random
import           Debug.Trace

{-
Example ASCII
 __ __ __ __ __ __ __ __ __ __ __ __ __ __ __
|__   |__       __ __|__   |   __|  |  |  |  |
|__   |__   |__|   __ __|   __ __      |     |
|        |  |  |     |  |__      |__|  |  |  |
|__|__|  |  |   __|   __|__   |   __|__|  |__|
|   __|  |     |__ __ __|  |  |__|  |     |  |
|  |  |  |  |__|  |__   |  |   __|__ __|  |  |
|  |__    __    __ __    __|  |   __   |  |  |
|  |  |  |  |      __|  |   __|  |  |__|  |  |
|  |     |     |__   |  |  |  |  |  |__    __|
|  |  |__|__|__ __|  |     |  |  |      __|  |
|__ __|  |  |  |__   |__|   __|     |   __ __|
|   __|  |   __|__      |__   |__|  |__    __|
|  |  |     |  |     |__|  |   __    __|   __|
|   __|  |__ __|__|      __|  |  |     |  |  |
|   __ __   |      __|__|  |__   |  |  |__|  |
|__ __ __|__ __|__ __ __ __ __|__|__|__ __ __|
-}

type Cell = (Bool, Bool) -- right, bottom
type Row = [Cell]
type Maze = [Row]

cellToStr :: Cell -> String
cellToStr (True , True ) = "__|"
cellToStr (False, True ) = "__ "
cellToStr (True , False) = "  |"
cellToStr (False, False) = "   "

sampleMaze :: Maze
sampleMaze =
  [ [(False, True), (False, False), (True, False)]
  , [(True, False), (True, True), (True, False)]
  , [(False, True), (True, True), (True, True)]
  ]

showRow :: Row -> String
showRow row = "|" ++ concatMap cellToStr row

showMazeTopLine :: Int -> String
showMazeTopLine mazeWidth = concat (replicate mazeWidth " __")

-- Maze's bottom side cells should always have bottom wall
-- Maze's right side cells should always have right wall
validateMaze :: Maze -> Bool
-- validateMaze maze = all snd (last maze) && all (fst . last) maze
validateMaze maze = True

showMaze :: Maze -> String
showMaze m = if validateMaze m
  then showMazeTopLine mazeWidth ++ "\n" ++ intercalate "\n" (map showRow m)
  else "Invalid maze"
  where mazeWidth = length $ head m

type Cell' = (Int, Cell)
type Row' = [Cell']

emptyRow :: Int -> Row'
emptyRow width = zip [0 ..] $ replicate width (False, False)

handleRowFunctor :: (StdGen, [Cell']) -> Cell' -> (StdGen, [Cell'])
handleRowFunctor (gen, acc) upRowCell'
  | null acc
  = (rngFloorGen, [(decideSet, (False, rngFloorValue))])
  | (lastSet == decideSet) || rngWallValue
  = ( rngFloorGen
    , init acc
      ++ [(lastSet, (True, snd lastCell)), (decideSet, (False, rngFloorValue))]
    )
  | otherwise
  = (rngFloorGen, acc ++ [(lastSet, (False, rngFloorValue))])

 where
  upRowCell     = snd upRowCell'
  upRowSet      = fst upRowCell'

  -- upRowCellSet  = fst upRowCell'
  -- if upRow has floor - set will be succ of max sets in acc
  decideSet     = if snd upRowCell then upRowSet + 100 else upRowSet

  rngWall       = randomR (True, False) gen
  rngWallGen    = snd rngWall
  rngWallValue  = fst rngWall
  rngFloor      = randomR (True, False) rngWallGen
  rngFloorGen   = snd rngFloor

  -- this ain't gonna work, we need at least one open floor for set
  rngFloorValue = fst rngFloor

  lastCell'     = last acc
  lastCell      = snd lastCell'
  lastSet       = fst lastCell'


handleRow :: StdGen -> Row' -> (StdGen, Row')
handleRow gen = foldl handleRowFunctor (gen, [])

generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze width height gen = map (map snd) rows' -- unwrap from Row' to Row
 where
  emptyRows' = replicate height
  rows'      = snd $ foldl
    (\(lgen, rows) _ ->
      let (ngen, row) =
              handleRow lgen (if null rows then emptyRow width else last rows)
      in  (ngen, rows ++ [row])
    )
    (gen, [])
    [0 .. height]

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn $ showMaze (generateMaze 20 20 gen)
