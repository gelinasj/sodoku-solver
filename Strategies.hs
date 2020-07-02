module Strategies
( strategies
) where
import Sodoku_Lang
import Board_Utils

strategies = map strategyWrapper unwrappedStrategies
unwrappedStrategies =
    [checkRow, checkCol, checkBox] ++ if isTradGame then [] else [checkUlDrDiag, checkDlUrDiag]

strategyWrapper :: Strategy -> Strategy
strategyWrapper strat =
    (\gb cell posn -> case cell of
        (Answer _) -> cell
        (Options _) -> strat gb cell posn)

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

checkUlDrDiag :: Strategy
checkUlDrDiag gb oldCell posn = checkDiag gb oldCell posn getUlDrDiag getUlDrPosns

checkDlUrDiag :: Strategy
checkDlUrDiag gb oldCell posn = checkDiag gb oldCell posn getDlUrDiag getDlUrPosns

checkDiag :: Gameboard -> Cell -> Position -> (Gameboard -> [Cell]) -> [Position] -> Cell
checkDiag gb oldCell posn@(Posn rowNum colNum) getDiag getPosns
    | onDiag = let diag = getDiag gb
               in checkVec oldCell (zip diag posns) posn
    | otherwise = oldCell
    where posns = getPosns
          onDiag = (length (setIntersect [posn] posns)) == 1

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
