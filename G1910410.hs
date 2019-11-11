-- | name: Nguyen Quoc Huy
-- | id: 1910410
-- | acknowledgements: 

import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


-- | -------------------------------------------------
-- | LAYER 1: COMPUTER GRAPHIC - GEOMETRIC COMPUTATION
-- | -------------------------------------------------

-- | This data structure stores an point on Oxy coordinate
-- | @arg: (x-coordinate value, y-coordinate value)
type MyPoint = (Float, Float)

-- | @return: Size of a square cell in integer
squareCellSizeInt :: Int
squareCellSizeInt = 20

-- | @return: Size of a square cell
squareCellSize :: Float
squareCellSize = fromIntegral squareCellSizeInt

-- | @return: The gap between size of the cell and the size of the part of cell that is actual colored
squareGap :: Float
squareGap = 2

-- | @param maxX: The maximum number of points on x-axis that we want to used in this plot
-- | @param maxY: The maximum number of points on y-axis that we want to used in this plot
-- | @param (x, y): The point that needs to be translated
-- | @return: The point after translation, and flipped along x-axis, so that the points in positive part
-- |          of the coordinate 
-- |   ^ y           ^ y            |     
-- |   |             |-----> x      |-----> x
-- |   |             |              |        
-- | --|--> x   ->   |         ->   |        
-- |   |             |              v (y)
originTranslation :: Float -> Float -> MyPoint -> MyPoint
originTranslation maxX maxY (x, y) = (x + (-maxX / 2), - (y + (-maxY / 2)))

-- | @param v: The origin point value on one-dimention axis
-- | @param dv: The distance which the origin point will be moved (in range of zero to one)
-- | @return: The point value after being moved by distance `dv`, scaled by `squareCellSize`, 
-- |          and then moved a little bit to the scaled center to create gap
pointToSegmentTip :: Float -> Float -> Float
pointToSegmentTip v dv = (v + dv) * squareCellSize + (1 - 2 * dv) * squareGap

-- | @return: The list of points that build up a one-unit square in counterclockwise, start from the origin
squarePositiveRoundDirection :: [(Float, Float)]
squarePositiveRoundDirection = [(0, 0), (0, 1), (1, 1), (1, 0)]

-- | @param maxX: The maximum number of points on x-axis that we want to used in this plot
-- | @param maxY: The maximum number of points on y-axis that we want to used in this plot
-- | @param (x, y): The coordinate of the cell we want to plot
-- | @return: The list of point to build a square - which is the point (pixel) (`x`, `y`) after scaling up
cellSquare :: Float -> Float -> MyPoint -> [MyPoint]
cellSquare maxX maxY (x, y) = [
        originTranslation maxX maxY (pointToSegmentTip x dx, pointToSegmentTip y dy)
            | (dx, dy) <- squarePositiveRoundDirection
    ]

-- | @param maxX: The maximum number of points on x-axis that we want to used in this plot
-- | @param maxY: The maximum number of points on y-axis that we want to used in this plot
-- | @param (x, y): The coordinate of the cell we want to plot
-- | @return: A Color object which presents square at (x, y) in color c
box :: Float -> Float -> MyPoint -> Color -> Picture
box maxX maxY (x, y) c = Color c (Polygon (cellSquare maxX maxY (x, y)))

-- | ------------------
-- | THE END OF LAYER 1
-- | ------------------


-- | -----------------------------------------------
-- | LAYER 2: CELL LEVEL - GRAPH THEORY ON GAMEBOARD
-- | -----------------------------------------------

-- | This data structure stores position of a cell on gameboard (torus plane)
-- | @arg: (Row index, Column index)
type CellPosition = (Int, Int)

-- | @return: A list representing eight unit directions, 
-- |          which are correspondently up, right, down, left, up-right, down-right, down-left, up-left
-- | 8    1    5
-- |  \   |   /
-- |    \ | /
-- | 4 ---x--- 2
-- |    / | \
-- |  /   |   \
-- | 7    3    6
adjacentEight :: [Int]
adjacentEight = [-1, 0, 1, 0, -1, 1, 1, -1, -1]

-- | @param nrows: Number of rows of gameboard
-- | @param ncols: Number of columns of gameboard
-- | @param (i, j): The position of a cell
-- | @return: The position in torus plane
torusMap :: Int -> Int -> CellPosition -> CellPosition
torusMap nrows ncols (i, j) = (((i `mod` nrows) + nrows) `mod` nrows, ((j `mod` ncols) + ncols) `mod` ncols)

-- | @param xs: A list
-- | @return: List `xs` without duplicated elements
dupRm :: Eq a => [a] -> [a]
dupRm []       = []
dupRm (x : xs) = x : dupRm [ y | y <- xs, y /= x ]

-- | @param (i, j): Row and colum index of the cell
-- | @param ds: List of unit directions
-- | @return: List of cells after moving according to the unit directions
adjacentMove :: CellPosition -> [Int] -> [CellPosition]
adjacentMove (i, j) []             = []
adjacentMove (i, j) (d : [])       = []
adjacentMove (i, j) (di : dj : ds) = (i + di, j + dj) : adjacentMove (i, j) (dj : ds)

-- | @param nrows: Number of rows 
-- | @param ncols: Number of cols
-- | @param (i, j): Row and column index
-- | @return: Cells that are adjacent to cell (i, j)
adjacentCell :: Int -> Int -> CellPosition -> [CellPosition]
adjacentCell nrows ncols (i, j) = dupRm (map (\u -> torusMap nrows ncols u) (adjacentMove (i, j) adjacentEight))

-- | @param nrows: Number of rows 
-- | @param ncols: Number of cols
-- | @param visited: Visited cells
-- | @return adjacents: Adjacent cells of visit cells (may dupplicate)
breathFirstSearch :: Int -> Int -> [CellPosition] -> [CellPosition]
breathFirstSearch nrows ncols visited = dupRm [
        v | u <- visited,
            v <- adjacentCell nrows ncols u,
            not (elem v visited)
    ]

-- | ------------------
-- | THE END OF LAYER 2
-- | ------------------


-- | ------------------------------
-- | LAYER 3: GAME STATE TRANSITION
-- | ------------------------------

-- | This data structure used to store the current state of the game of life
-- | by storing list of live cells positions in form of (i-th row, j-th column), 
-- | and also store the size of gameboard by number of rows and number of columns
 
-- | @arg: (Number of rows, Number of columns, Live cells)
type State = (Int, Int, [CellPosition])

-- | @param (nrows, ncols, lives): The current state
-- | @return: The cells which is living and will continue living in next state
-- |          These cells are cells that have exactly 2 or 3 living neighbours
livesRemain :: State -> [CellPosition]
livesRemain (nrows, ncols, lives) = 
    filter (\u -> elem (length [v | v <- adjacentCell nrows ncols u, elem v lives]) [2, 3]) lives

-- | @param (nrows, ncols, lives): The current state
-- | @return: The cells which is not living but will be born in next state
-- |          These cells are cells that have exactly 3 living neighbours
newBorn :: State -> [CellPosition]
newBorn (nrows, ncols, lives) = 
    filter (\u -> length [v | v <- adjacentCell nrows ncols u, elem v lives] == 3) (breathFirstSearch nrows ncols lives)

-- | @param (nrows, ncols, lives): The current state
-- | @return: The state after this state
nextState :: State -> State
nextState (nrows, ncols, lives) = (
        nrows, 
        ncols,
        livesRemain (nrows, ncols, lives) ++ newBorn (nrows, ncols, lives)
    )

-- | @param (nrows, ncols, cells): A state
-- | @return: Picture of current state
-- | Note: Row axis in the table is y-axis, while column axis is x-axis
draw :: State -> Picture
draw (nrows, ncols, cells) = 
    Pictures [
        box 
            ((fromIntegral ncols) * squareCellSize) 
            ((fromIntegral nrows) * squareCellSize) 
            (fromIntegral j, fromIntegral i) 
            (if (elem (i, j) cells) then red else blue) | i <- [0 .. (nrows-1)],
                                                          j <- [0 .. (ncols-1)]
    ]

-- | @param _: ViewPort *not be used in this implementation*
-- | @param _: Amount of time for this simulation *not be used in this implementation*
-- | @param state: The current state
-- | @return: The state after this state
next :: ViewPort -> Float -> State -> State
next _ _ state  = nextState state

-- | @param nrows: Number of rows of gameboard
-- | @param ncols: Number of columns of gameboard
-- | @return: A window for the gameboard of `nrows` rows and `ncols` colums
window :: Int -> Int -> Display
window nrows ncols = InWindow "Game of Life" (ncols * squareCellSizeInt, nrows * squareCellSizeInt) (100, 100)

-- | @return: Number of frames per second
fps :: Int
fps = 2

-- | @param c: Input character
-- | @return: True whether `c` is character presenting a live cell which is '#'
isLiveCharacter :: Char -> Bool
isLiveCharacter c = c == '#'

-- | @param mss: A state presented by text matrix
-- | @return: A state corresponding to the text
cellsFromTxt :: [[Char]] -> State
cellsFromTxt mss = (
        length mss,
        foldl max 0 [length ms | ms <- mss],
        [(i, j) | (i, ms) <- zip [0..] mss,
                  (j, m) <- zip [0..] ms,
                  isLiveCharacter m]
    )

-- | ------------------
-- | THE END OF LAYER 3
-- | ------------------


-- | @param step: The current step
-- | @param state: The current state
-- | @return: Nothing, just print the state from current state to the end, at most 1000 steps
printStates step (nrows, ncols, cells) = do
    print cells
    printStates (step + 1) (nextState (nrows, ncols, cells))

-- | @param args: A list containing the command-line arguments of main program
-- | @return: An Maybe object that whether the program is in debug mode or not (Nothing means not)
isDebugMode []                     = Nothing
isDebugMode (x : _) | x == "DEBUG" = Just x
                    | otherwise    = Nothing

main = do
    file : otherArgs <- getArgs
    text <- readFile file
    let (nrows, ncols, cells) = cellsFromTxt (lines text)
    case isDebugMode otherArgs of 
        Just _ -> printStates 0 (nrows, ncols, cells)
        Nothing -> simulate (window nrows ncols) (dark white) fps (nrows, ncols, cells) draw next

