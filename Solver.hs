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
              if gameSolved gb' || (optionIndex == ((length freeSet) - 1))
                  then gb'
                  else exploreOption (optionIndex + 1)
              where newCell = Answer (freeSet !! optionIndex)
                    gb' = solve (replaceCell gb posn newCell)

genViewBoard :: [Int] -> Gameboard
genViewBoard showSet = (first:rest)
    where showAnswers = map (\val -> Answer val) showSet
          fill = replicate (9 - (length showSet)) (Answer 0)
          first = showAnswers ++ fill
          rest = replicate 8 (replicate 9 (Answer 0))


updateFreeSets :: Gameboard -> Gameboard
updateFreeSets gb = foldl updateFreeSet gb itr
    where itr = initIterator gb
          updateFreeSet :: Gameboard -> (Cell, Position) -> Gameboard
          updateFreeSet gbAcc ((Options _), posn@(Posn row col)) =
              replaceCell gbAcc posn newCell
              where rowFreeSet = getVecFreeSet (getRow gb row)
                    colFreeSet = getVecFreeSet (getCol gb col)
                    boxFreeSet = getBoxFreeSet (getBox gb row col)
                    traditionalFreeSet = setIntersect boxFreeSet (setIntersect rowFreeSet colFreeSet)
                    updatedFreeSet = if isTradGame
                        then traditionalFreeSet
                        else setIntersect traditionalFreeSet (getDiagFreeSet gb row col)
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
\|  3   2   9  |  5   8   4  |  7   1   6  |\
\|  5   1   6  |  7   9   3  |  2   8   4  |\
\|  7   4   8  |  2   1   6  |  5   9   3  |\
\|-----------------------------------------|\
\|  4   9   1  |  3   6   5  |  8   2   7  |\
\|  8   5   2  |  4   7   1  |  3   6   9  |\
\|  6   3   7  |  9   2   8  |  4   5   1  |\
\|-----------------------------------------|\
\|  2   8   3  |  1   4   9  |  6   7   5  |\
\|  9   6   5  |  8   3   7  |  1   4   2  |\
\|  1   7   4  |  6   5   2  |  9   3   8  |\
\-------------------------------------------\n"

genTestBoard :: Gameboard
genTestBoard = parseBoard testBoardStr
