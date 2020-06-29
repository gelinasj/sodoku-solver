module Solver
( solve
, updateBoardState
) where
import Sodoku_Lang
import Board_Utils
import Parse_Unparse

solve :: Gameboard -> Gameboard
solve gb = gb
solve gb = doWhile (not . gameSolved) updateBoardState gb

doWhile :: (a -> Bool) -> (a -> a) -> a -> a
doWhile predicate action state =
    if (predicate state)
        then doWhile predicate action (action state)
        else state

updateFreeSets :: Gameboard -> Gameboard
updateFreeSets gb = gbUpdated
    where gbUpdated = foldl refreshFreeSet gb (initIterator gb)
          refreshFreeSet :: Gameboard -> (Cell, Position) -> Gameboard
          refreshFreeSet gbAcc (oldCell@(Options _), posn@(Posn row col)) =
              replaceCell gbAcc oldCell newCell
              -- replaceCell gbAcc posn newCell
              where rowFreeSet = getVecFreeSet (getRow gb row)
                    colFreeSet = getVecFreeSet (getCol gb col)
                    boxFreeSet = getBoxFreeSet (getBox gb row col)
                    updatedFreeSet = setIntersect boxFreeSet (setIntersect rowFreeSet colFreeSet)
                    newCell = Options updatedFreeSet
          refreshFreeSet gbAcc ((Answer freeSet), _) = gbAcc


testBoardStr = "\
\_2_ ___ _3_\
\__3 5__ _1_\
\___ _3_ ___\
\7_2 __6 ___\
\___ ___ __7\
\1__ _9_ ___\
\___ ___ 1__\
\___ _7_ ___\
\_8_ ___ ___"

genTestBoard :: Gameboard
genTestBoard = parseBoard testBoardStr
