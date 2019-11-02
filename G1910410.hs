-- | Student Name: Nguyen Quoc Huy
-- | Student Number: 1910410
-- | Acknowledgement: 

import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


-- | Computer graphic - Geometric layer 
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
originTranslation:: Float -> Float -> MyPoint -> MyPoint
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

-- | @arg: (Number of rows, Number of columns, Live/Dead Cells)
type State = (Int, Int, [[Bool]])

-- | Draw a state
draw :: State -> Picture
draw (nrows, ncols, bss) =
    Pictures [
        box maxX maxY (x, y) (if b then red else blue) | let maxX = (fromIntegral ncols) * squareCellSize,
                                                         let maxY = (fromIntegral nrows) * squareCellSize,
                                                         (i, bs) <- zip [0..] bss,
                                                         (j, b)  <- zip [0..] bs,
                                                         let x = fromIntegral j,
                                                         let y = fromIntegral i 
    ]

-- | Change from a state to another state, known ViewPort, amount of time for this simulation step and current state
next :: ViewPort -> Float -> State -> State
next _ _ (nrows, ncols, bss) = (nrows, ncols, [ 
    [ not b | b <- bs ] | bs <- bss 
  ])

-- | New window (Display)
window nrows ncols = InWindow "Game of Life" (ncols * squareCellSizeInt, nrows * squareCellSizeInt) (100, 100)

-- | Frames per second
fps :: Int
fps = 1

isLiveCharacter :: Char -> Bool
isLiveCharacter v = v == '#'

cellsFromTxt :: [[Char]] -> State
cellsFromTxt mss = (
        length mss,
        foldl max 0 [length ms | ms <- mss],
        [ [ isLiveCharacter v | v <- ms ] | ms <- mss ]
    )

main = do
    file : _ <- getArgs
    text <- readFile file
    let (nrows, ncols, bss) = cellsFromTxt (lines text)
    simulate (window nrows ncols) (dark white) fps (nrows, ncols, bss) draw next

