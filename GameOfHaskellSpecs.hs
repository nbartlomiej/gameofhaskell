module GameOfHaskell.Specs where

import qualified GameOfHaskell as G
import Test.Hspec

containsAll :: Eq a => [a] -> [a] -> Bool
list `containsAll` elements = all (\x->x == True) ( map (\x->x `elem` list) elements)

specs :: Specs
specs = describe "Application" [
    describe "Integration" [
      it "Simulates Conway's Game of Life" (
        let board = [(-1,0),(0,0), (1,0)]
            next = G.nextState board
            expectedResults =  [(0,1),(0,0),(0,-1)]
        in  next `containsAll` expectedResults
      )
    ],
    describe "Board" [
      describe "neighbours" [
        it "Calculates neighbours for a single cell" (
          let expectedResults = [
                ((-1,-1),1), ((-1,0),1), ((-1,1),1),
                ((0,-1),1),  ((0,0),1),  ((0,1),1),
                ((1,-1),1),  ((1,0),1),  ((1,1),1) ]
          in  (G.neighbours [(0,0)]) `containsAll` expectedResults
        ),
        it "Calculates neighbours for multiple cells" (
          G.neighbours [(0,0),(2,2)] `containsAll` [((1,1),2)]
        )
      ],
      describe "nextState" [
        it "Creates new cells" (
          (1,1) `elem` (G.nextState [(0,0),(0,1),(1,0)])
        ),
        it "Removes dead cells" (
          (0,0) `notElem` (G.nextState [(0,0)])
        )
      ]
    ]
  ]

main = hspec specs

