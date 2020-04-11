module Main where

import           Data.List
import           System.Random
import           Debug.Trace
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Category               ( (>>>) )


{-
http://www.neocomputer.org/projects/eller.html

1. Create the first row. No cells will be members of any set

2. Join any cells not members of a set to their own unique set

3. Create right-walls, moving from left to right:
  Randomly decide to add a wall or not
    If the current cell and the cell to the right are members of the same set, always create a wall between them. (This prevents loops)
    If you decide not to add a wall, union the sets to which the current cell and the cell to the right are members.

4. Create bottom-walls, moving from left to right:
  Randomly decide to add a wall or not. Make sure that each set has at least one cell without a bottom-wall (This prevents isolations)
    If a cell is the only member of its set, do not create a bottom-wall
    If a cell is the only member of its set without a bottom-wall, do not create a bottom-wall

5. Decide to keep adding rows, or stop and complete the maze
  If you decide to add another row:
    Output the current row
    Remove all right walls
    Remove cells with a bottom-wall from their set
    Remove all bottom walls
    Continue from Step 2

  If you decide to complete the maze
    Add a bottom wall to every cell
    Moving from left to right:
      If the current cell and the cell to the right are members of a different set:
        Remove the right wall
        Union the sets to which the current cell and cell to the right are members.
        Output the final row
-}

data Cell = Cell { left :: Maybe Bool,  bottom :: Maybe Bool } deriving (Show, Eq)
type Row = [Cell]

type Cell' = (Maybe Int, Cell)
type Row' = [Cell']

verticalWallChance = 50
horizontalWallChance = 50

sampleRow' :: Row'
sampleRow' =
  [ (Nothing , Cell { left = Just False, bottom = Just False })
  , (Just 344, Cell { left = Just False, bottom = Just False })
  , (Nothing , Cell { left = Just False, bottom = Just False })
  ]

assignUniqueSets :: Row' -> Row'
assignUniqueSets row = snd $ mapAccumL
  (\x (set, cell) ->
    if isNothing set then (x + 1, (Just x, cell)) else (x, (set, cell))
  )
  nextUniqSet
  row
 where
  sets        = mapMaybe fst row
  nextUniqSet = if null sets then 0 else maximum sets + 1000 + 1

verticalWallsF :: Row' -> (Bool, Cell') -> Row'
verticalWallsF acc (rngAddWall, upRowCell')
  | null acc
  = [upRowCell']
  | fst (last acc) == fst upRowCell' || rngAddWall
  = acc ++ [(fst upRowCell', (snd upRowCell') { left = Just True })]
  | otherwise
  = acc ++ [(fst (last acc), snd upRowCell')]

removeVerticalWalls :: Row' -> Row'
removeVerticalWalls = map
  (\(set, cell) ->
    (set, if isJust (left cell) then cell { left = Just False } else cell)
  )

removeHorizontalWalls :: Row' -> Row'
removeHorizontalWalls = map
  (\(set, cell) ->
    ( if bottom cell == Just True then Nothing else set
    , cell { bottom = Just False }
    )
  )

allHorizontalWallsForSet :: Maybe Int -> Row' -> Bool
allHorizontalWallsForSet set row = and $ mapMaybe (bottom . snd) setCells
 where
  setCells       = filter (\x -> fst x == set) row
  setBottomWalls = mapMaybe (bottom . snd) setCells

horizontalWallsF :: Int -> Row' -> (Bool, Cell') -> Row'
horizontalWallsF rowWidth acc (rngAddWall, cell')
  | not (null acc) && fst (last acc) /= fst cell'
  = let
      prevCell'           = last acc
      (prevSet, prevCell) = prevCell'
      prevCellNewBottomWall =
        not (allHorizontalWallsForSet prevSet acc)
          && fromMaybe True (bottom prevCell)
    in
      init acc
        ++ [ (prevSet  , prevCell { bottom = Just prevCellNewBottomWall })
           , (fst cell', (snd cell') { bottom = Just currentCellBottom })
           ]
  | otherwise
  = acc ++ [(fst cell', (snd cell') { bottom = Just currentCellBottom })]
 where
  isLastCell        = length acc + 1 == rowWidth
  currentCellBottom = if isLastCell
    then not (allHorizontalWallsForSet (fst cell') acc && rngAddWall)
    else rngAddWall



preIteration =
  removeVerticalWalls >>> removeHorizontalWalls >>> assignUniqueSets

handleRow :: StdGen -> Row' -> Row'
handleRow gen row =
  let preIterationResult = preIteration row
      afterVerticalWallsF =
          foldl verticalWallsF [] $ zip rngVerticalB preIterationResult
      afterHorizontalWallsF = foldl (horizontalWallsF (length row)) []
        $ zip rngHorizontalB afterVerticalWallsF
  in  afterHorizontalWallsF
 where
  (verticalGen, horizontalGen) = split gen
  rngVertical                  = randomRs (0 :: Int, 100 :: Int) verticalGen
  rngVerticalB                 = map (<= verticalWallChance) rngVertical
  rngHorizontal                = randomRs (0 :: Int, 100 :: Int) horizontalGen
  rngHorizontalB               = map (<= horizontalWallChance) rngHorizontal

firstRow :: Int -> Row'
firstRow width =
  (Nothing, Cell { left = Nothing, bottom = Just False }) : replicate
    (width - 1)
    (Nothing, Cell { left = Just False, bottom = Just False })

generateMaze :: Int -> Int -> StdGen -> [Row']
generateMaze width height gen = snd $ foldl
  (\(lgen, rows) _ ->
    let (g1, g2) = split lgen
    in  ( g1
        , rows
          ++ [handleRow g2 (if null rows then firstRow width else last rows)]
        )
  )
  (gen, [])
  [0 .. height]


ec = Cell { left = Nothing, bottom = Nothing }

-- stack ghci -> unitTests (:reload as needed)
unitTests = defaultMain $ testGroup
  "Main"
  [ testGroup
    "assignUniqueSets"
    [ testCase "Uses 0 as first set"
    $   assignUniqueSets [(Nothing, ec)]
    @?= [(Just 0, ec)]
    , testCase "Increments set for next Nothing"
    $   assignUniqueSets [(Nothing, ec), (Just 0, ec)]
    @?= [(Just 1, ec), (Just 0, ec)]
    , testCase "Will assign unique set for each Nothing"
    $   assignUniqueSets [(Nothing, ec), (Just 10, ec), (Nothing, ec)]
    @?= [(Just 11, ec), (Just 10, ec), (Just 12, ec)]
    ]
  , testGroup
    "verticalWallsF"
    [ testCase "Adds a wall between same-set cells with rng False"
    $   verticalWallsF [(Just 10, ec)] (False, (Just 10, ec))
    @?= [(Just 10, ec), (Just 10, ec { left = Just True })]
    , testCase "Unions sets if not adding a wall"
    $   verticalWallsF [(Just 10, ec)] (False, (Just 11, ec))
    @?= [(Just 10, ec), (Just 10, ec)]
    , testCase "Adds a wall if add wall is True"
    $   verticalWallsF [(Just 10, ec)] (True, (Just 11, ec))
    @?= [(Just 10, ec), (Just 11, ec { left = Just True })]
    , testCase "Does nothing with first cell if add wall is True"
    $   verticalWallsF [] (True, (Just 10, ec))
    @?= [(Just 10, ec)]
    , testCase "Does nothing with first cell if add wall is False"
    $   verticalWallsF [] (False, (Just 10, ec))
    @?= [(Just 10, ec)]
    ]
  , testGroup
    "removeVerticalWalls"
    [ testCase "Removes all vertical walls"
    $   removeVerticalWalls
          [ (Just 10 , Cell { left = Just True, bottom = Nothing })
          , (Just 100, Cell { left = Just True, bottom = Nothing })
          ]
    @?= [ (Just 10 , Cell { left = Just False, bottom = Nothing })
        , (Just 100, Cell { left = Just False, bottom = Nothing })
        ]
    , testCase "Ignores Nothing"
    $ removeVerticalWalls [(Just 10, Cell { left = Nothing, bottom = Nothing })]
    @?= [(Just 10, Cell { left = Nothing, bottom = Nothing })]
    ]
  , testGroup
    "removeHorizontalWalls"
    [ testCase "Removes all horizontal walls and sets set to Nothing if removed"
      $   removeHorizontalWalls
            [ (Just 10 , Cell { left = Nothing, bottom = Just True })
            , (Just 100, Cell { left = Nothing, bottom = Just False })
            ]
      @?= [ (Nothing , Cell { left = Nothing, bottom = Just False })
          , (Just 100, Cell { left = Nothing, bottom = Just False })
          ]
    ]
  , testGroup
    "horizontalWallsF"
    [ testCase "Adds a wall based on RNG value"
    $   horizontalWallsF
          100
          [(Just 10, Cell { left = Nothing, bottom = Just False })]
          (True, (Just 11, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just False })
        , (Just 11, Cell { left = Nothing, bottom = Just True })
        ]
    , testCase "Removes wall if prev cell is the only one in the set"
    $   horizontalWallsF
          100
          [(Just 10, Cell { left = Nothing, bottom = Just True })]
          (True, (Just 11, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just False })
        , (Just 11, Cell { left = Nothing, bottom = Just True })
        ]
    , testCase "Removes a wall in prev cell if prev set has no pass down"
    $   horizontalWallsF
          100
          [ (Just 10, Cell { left = Nothing, bottom = Just True })
          , (Just 10, Cell { left = Nothing, bottom = Just True })
          ]
          (True, (Just 11, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just True })
        , (Just 10, Cell { left = Nothing, bottom = Just False })
        , (Just 11, Cell { left = Nothing, bottom = Just True })
        ]
    , testCase "Leaves a wall in prev cell if prev set has a pass down"
    $   horizontalWallsF
          100
          [ (Just 10, Cell { left = Nothing, bottom = Just False })
          , (Just 10, Cell { left = Nothing, bottom = Just True })
          ]
          (True, (Just 11, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just False })
        , (Just 10, Cell { left = Nothing, bottom = Just True })
        , (Just 11, Cell { left = Nothing, bottom = Just True })
        ]
    , testCase "Last cell doesn't leave closed room"
    $   horizontalWallsF
          3
          [ (Just 10, Cell { left = Nothing, bottom = Just True })
          , (Just 10, Cell { left = Nothing, bottom = Just True })
          ]
          (True, (Just 10, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just True })
        , (Just 10, Cell { left = Nothing, bottom = Just True })
        , (Just 10, Cell { left = Nothing, bottom = Just False })
        ]
    , testCase "Last cell doesn't leave closed room when it's the only member"
    $   horizontalWallsF
          2
          [(Just 10, Cell { left = Nothing, bottom = Just False })]
          (True, (Just 11, Cell { left = Nothing, bottom = Just False }))
    @?= [ (Just 10, Cell { left = Nothing, bottom = Just False })
        , (Just 11, Cell { left = Nothing, bottom = Just False })
        ]
    ]
  ]


cellToStr :: (Bool, Bool) -> String -- bottom, left
cellToStr (True , True ) = "|__"
cellToStr (False, True ) = "|  "
cellToStr (True , False) = " __"
cellToStr (False, False) = "   "

showRow :: Row -> String
showRow = (++ "|") . concatMap
  (cellToStr . (\x -> (fromMaybe True (bottom x), fromMaybe True (left x))))


showMazeTopLine :: Int -> String
showMazeTopLine mazeWidth = " " ++ concat (replicate mazeWidth "__ ")

showMaze :: [Row] -> String
showMaze m = showMazeTopLine mazeWidth ++ "\n" ++ intercalate "\n"
                                                              (map showRow m)
  where mazeWidth = length $ head m


main = do
  gen <- getStdGen
  putStrLn (showMaze $ map (map snd) $ generateMaze 10 10 gen)
