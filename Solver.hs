module Solver
( solve
, updateFreeSets
, runStrategies
) where
import Sodoku_Lang
import Board_Utils
import Parse_Unparse
import Strategies as Sodoku

solve :: Gameboard -> Gameboard
solve = whileDo playable updateGameState

playable :: Gameboard -> Bool
playable gb = (not . gameSolved) gb && potentialMoves gb

whileDo :: (a -> Bool) -> (a -> a) -> a -> a
whileDo predicate action state =
    if (predicate state)
        then whileDo predicate action (action state)
        else state

updateGameState :: Gameboard -> Gameboard
updateGameState gb = if gb /= gb'
                        then gb'
                        else exploreOptions gb' 1
    where gb' = (runStrategies . updateFreeSets) gb

exploreOptions :: Gameboard -> Int -> Gameboard
exploreOptions gb optionSize =
    case maybeOption of
        (Just ((Options freeSet), posn)) ->
            determineCorrectOption gb freeSet posn
        Nothing -> exploreOptions gb (optionSize + 1)
    where maybeOption = getCellOfOptionSize gb optionSize

determineCorrectOption :: Gameboard -> FreeSet -> Position -> Gameboard
determineCorrectOption gb freeSet posn =
    exploreOption 0
    where exploreOption :: Int -> Gameboard
          exploreOption optionIndex =
              if gameSolved gb'
                  then gb'
                  else exploreOption (optionIndex + 1)
              where newCell = Answer (freeSet !! optionIndex)
                    gb' = solve (replaceCell gb posn newCell)

updateFreeSets :: Gameboard -> Gameboard
updateFreeSets gb = foldl updateFreeSet gb itr
    where itr = initIterator gb
          updateFreeSet :: Gameboard -> (Cell, Position) -> Gameboard
          updateFreeSet gbAcc ((Options _), posn@(Posn row col)) =
              replaceCell gbAcc posn newCell
              where rowFreeSet = getVecFreeSet (getRow gb row)
                    colFreeSet = getVecFreeSet (getCol gb col)
                    boxFreeSet = getBoxFreeSet (getBox gb row col)
                    --updatedFreeSet = rowFreeSet ++ [-1] ++ colFreeSet ++ [-1] ++ boxFreeSet
                    updatedFreeSet = setIntersect boxFreeSet (setIntersect rowFreeSet colFreeSet)
                    newCell = Options updatedFreeSet
          updateFreeSet gbAcc ((Answer freeSet), _) = gbAcc

runStrategies :: Gameboard -> Gameboard
runStrategies gb = foldl genAnswerIfPossible gb (initIterator gb)
    where genAnswerIfPossible :: Gameboard -> (Cell, Position) -> Gameboard
          genAnswerIfPossible gbAcc (oldCell, posn) = replaceCell gbAcc posn newCell
              where newCell = foldl runStrategy oldCell Sodoku.strategies
                    runStrategy :: Cell -> Strategy -> Cell
                    runStrategy cellAcc strat = strat gb cellAcc posn

------------------------
--    Test Helpers    --
------------------------

testBoardStr = "\
\-------------------------------------------\
\|  _   _   8  |  _   _   _  |  3   _   _  |\
\|  _   _   _  |  _   1   4  |  _   9   _  |\
\|  _   _   _  |  _   _   2  |  _   8   4  |\
\|-----------------------------------------|\
\|  _   9   2  |  6   _   _  |  _   _   _  |\
\|  _   7   4  |  _   _   _  |  _   _   _  |\
\|  1   _   _  |  _   _   _  |  _   3   5  |\
\|-----------------------------------------|\
\|  _   6   _  |  5   8   7  |  _   4   _  |\
\|  _   _   _  |  9   _   _  |  _   _   _  |\
\|  _   _   _  |  _   4   _  |  5   _   _  |\
\-------------------------------------------"

genTestBoard :: Gameboard
genTestBoard = parseBoard testBoardStr
