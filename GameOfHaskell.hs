-- Simple Conway's Game of Life implementation

module GameOfHaskell where
import Data.List

type Cell = (Int, Int)
type LivingCell = Cell

-- We're tracking only alive cells.
type Board = [LivingCell]

nextState :: Board -> Board
nextState board =
  -- Newborn: a cell not living before having exactly 3 living neighbours now.
  let newborn  ((x,y), neighbours) = (x,y) `notElem` board && neighbours == 3
  -- Survivor: present on board before, has 3 to 4 living neighbours now.
      survivor ((x,y), neighbours) = (x,y) `elem` board && (neighbours `elem` [2,3])
  -- For every cell that has living neighbours, if the number of living
  -- neighbours suffices to be a living cell - become a living cell.
  in  [ c | (c,n) <- (livingNeighbours board), newborn (c,n) || survivor (c,n)]

livingNeighbours :: Board -> [(Cell, Int)]
livingNeighbours board =
  let deltas = [ (-1,-1),(0,-1),(1,-1),
                 (-1, 0),       (1, 0),
                 (-1, 1),(0, 1),(1, 1) ]
  -- List of cells that have a living neighbour. Contains duplicates.
      allNeighbours = concat $ map (\(x,y) -> map (\(a,b) -> (x+a,y+b)) deltas ) board
  -- Aggregated list of cells that have living neighbour(s) in format:
  -- (coordinates, neighbourCount).
  in (map (\xs@(x:_) -> (x, length xs)) . group . sort) allNeighbours 

-- I/O handling, below.

main = play [(0,0),(0,1),(0,2),(1,2),(2,1)]

play :: Board -> IO ()
play board = do
  display board
  putStrLn "Press return to continue..."
  getLine
  play $ nextState board

display :: Board -> IO ()
display board = do
  mapM_ (\x -> do
    mapM_ (\y -> putChar $ if (x,y) `elem` board then 'x' else ' ') [-10 .. 10]
    putChar '\n'
    ) [-10 .. 2]
