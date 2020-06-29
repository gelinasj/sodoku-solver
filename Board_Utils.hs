module Board_Utils
( getRow
, getCol
, getCell
, getBox
, initIterator
, getBoxFreeSet
, getVecFreeSet
, setIntersect
, gameSolved
, replaceCell
) where
import Sodoku_Lang

replaceCell :: Gameboard -> Cell -> Cell -> Gameboard
replaceCell gb oldCell newCell = [[cell | gbCell <- row,
                                         let cell = if gbCell == oldCell
                                                        then newCell
                                                        else gbCell]
                                        | row <- gb]

gameSolved :: Gameboard -> Bool
gameSolved gb = all isAnswer (concat gb)
    where isAnswer :: Cell -> Bool
          isAnswer (Answer _) = True
          isAnswer _ = False

initIterator :: Gameboard -> GameboardIterator
initIterator gb = [(getCell gb pos, pos) | row <- [0..8],
                                           col <- [0..8],
                                           let pos = Posn row col]

getRow :: Gameboard -> Int -> Row
getRow gb rowNum = gb !! rowNum

getCol :: Gameboard -> Int -> Column
getCol gb colNum = map (\row -> row !! colNum) gb

getCell :: Gameboard -> Position -> Cell
getCell gb (Posn row col) = gb !! row !! col

getBox :: Gameboard -> Int -> Int -> Box
getBox gb boxRow boxCol = [[getCell gb (Posn row col) |
                                col <- (take 3 [(3*(mod boxCol 3))..])] |
                                row <- (take 3 [(3*(mod boxRow 3))..])]

getBoxFreeSet :: Box -> FreeSet
getBoxFreeSet box = sodokuSetComplement boxVals
    where boxVals = [val | (Answer val) <- (concat box)]

getVecFreeSet :: [Cell] -> FreeSet
getVecFreeSet vec = sodokuSetComplement vecVals
    where vecVals = [val | (Answer val) <- vec]

sodokuSetComplement :: UsedSet -> FreeSet
sodokuSetComplement usedSet = setComplement [1..9] usedSet

setIntersect :: (Eq a) => [a] -> [a] -> [a]
setIntersect set1 set2 = [val | val <- set1, elem val set2]

setComplement :: (Eq a) => [a] -> [a] -> [a]
setComplement universe set = [uniVal | uniVal <- universe,
                                       not (elem uniVal set)]
