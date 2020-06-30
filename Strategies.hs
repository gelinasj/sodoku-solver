module Strategies
( strategies
, checkRow
, checkCol
, checkBox
, checkVec
, getNewCell
) where
import Sodoku_Lang
import Board_Utils

strategies = map strategyWrapper unwrappedStrategies
unwrappedStrategies = [onlyChoice, checkRow, checkCol, checkBox]

strategyWrapper :: Strategy -> Strategy
strategyWrapper strat =
    (\gb cell posn -> case cell of
        (Answer _) -> cell
        (Options _) -> strat gb cell posn)

onlyChoice :: Strategy
onlyChoice gb (Options [answer]) posn = Answer answer
onlyChoice gb oldCell posn = oldCell

checkRow :: Strategy
checkRow gb oldCell posn@(Posn rowNum _) =
    checkVec oldCell (zip row posns) posn
    where row = getRow gb rowNum
          posns = [(Posn rowNum col) | col <- [0..8]]

checkCol :: Strategy
checkCol gb oldCell posn@(Posn _ colNum) =
    checkVec oldCell (zip col posns) posn
    where col = getCol gb colNum
          posns = [(Posn row colNum) | row <- [0..8]]

checkBox :: Strategy
checkBox gb oldCell posn@(Posn rowNum colNum) =
    checkVec oldCell (zip box posns) posn
    where box = concat (getBox gb rowNum colNum)
          posns = [(Posn row col) | row <- (take 3 [(3*(div rowNum 3))..]),
                                    col <- (take 3 [(3*(div colNum 3))..])]
checkVec :: Cell -> [(Cell, Position)] -> Position -> Cell
checkVec oldCell vec oldCellPosn = getNewCell oldCell vecFreeSet
    where vecFreeSet = concat [freeVals | ((Options freeVals), posn) <- vec,
                                          posn /= oldCellPosn]

getNewCell :: Cell -> [Int] -> Cell
getNewCell oldCell@(Options freeSet) groupFreeSet =
    case maybeAnswer of
        [] -> oldCell
        [answer] -> Answer answer
        otherwise -> oldCell
    where maybeAnswer = (setIntersect freeSet (sodokuSetComplement groupFreeSet))
