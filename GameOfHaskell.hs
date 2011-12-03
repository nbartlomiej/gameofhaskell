module GameOfHaskell where
import Data.List
import Control.Monad

type Cell = (Int, Int)
type Board = [Cell]

nextState :: Board -> Board
nextState board =
  let newborn ((x,y),n)  = (x,y) `notElem` board && n == 3
      survivor ((x,y),n) = (x,y) `elem` board && (n==3 || n==4)
  in  [ cell | (cell,n) <- (neighbours board), newborn (cell,n) || survivor (cell,n)]

neighbours :: Board -> [(Cell, Int)]
neighbours board =
  let deltas = [
         (-1,-1),(0,-1),(1,-1),
         (-1, 0),(0, 0),(1, 0),
         (-1, 1),(0, 1),(1, 1)]
      allNeighbours = concat $ map (\(x,y) -> map (\(a,b) -> (x+a,y+b)) deltas ) board
  in (map (\xs@(x:_) -> (x, length xs)) . group . sort) allNeighbours 

-- -- -- -- -- --

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
    mapM_ (\y ->
        putChar $ if (x,y) `elem` board then 'x' else ' '
      ) [-10 .. 10]
    putChar '\n'
    ) [-10 .. 2]
