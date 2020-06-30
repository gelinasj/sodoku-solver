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
, sodokuSetComplement
, setUnion
, getCellOfOptionSize
, potentialMoves
, checkSolution
) where
import Data.List
import Sodoku_Lang

replaceCell :: Gameboard -> Position -> Cell -> Gameboard
replaceCell gb posn newCell = replaceCellRow gb (Posn 0 0) posn newCell

replaceCellRow :: Gameboard -> Position -> Position -> Cell -> Gameboard
replaceCellRow (row:rest) currPosn@(Posn currRow currCol) repPosn@(Posn repRow _) newCell
    | currRow == repRow = ((replaceCellCol row currPosn repPosn newCell):rest)
    | otherwise = (row:(replaceCellRow rest (Posn (currRow + 1) currCol) repPosn newCell))

replaceCellCol :: Row -> Position -> Position -> Cell -> Row
replaceCellCol (col:rest) currPosn@(Posn currRow currCol) repPosn@(Posn _ repCol) newCell
    | currCol == repCol = (newCell:rest)
    | otherwise = (col:(replaceCellCol rest (Posn currRow (currCol + 1)) repPosn newCell))

checkSolution :: Gameboard -> Bool
checkSolution gb = all correctlySolvedVecs [0..8]
    where correctlySolvedVecs :: Int -> Bool
          correctlySolvedVecs index =
              (noDups row) && (noDups col) && (noDups box)
              where row = getRow gb index
                    col = getCol gb index
                    box = concat (getBox gb (div index 3) (mod index 3))
                    noDups vec = (length (rmdups vec)) == (length vec)

gameSolved :: Gameboard -> Bool
gameSolved gb = all isAnswer (concat gb)
    where isAnswer :: Cell -> Bool
          isAnswer (Answer _) = True
          isAnswer _ = False

potentialMoves :: Gameboard -> Bool
potentialMoves gb = all optionHasMove (initIterator gb)
    where optionHasMove :: (Cell, Position) -> Bool
          optionHasMove ((Options freeSet), _) = (length freeSet) > 0
          optionHasMove _ = True

initIterator :: Gameboard -> GameboardIterator
initIterator gb = [(getCell gb pos, pos) | row <- [0..8],
                                           col <- [0..8],
                                           let pos = Posn row col]

getCellOfOptionSize :: Gameboard -> Int -> Maybe (Cell, Position)
getCellOfOptionSize gb optionSize = find matchesSize (initIterator gb)
    where matchesSize :: (Cell, Position) -> Bool
          matchesSize ((Options freeSet), _) = (length freeSet) == optionSize
          matchesSize _ = False

getRow :: Gameboard -> Int -> Row
getRow gb rowNum = gb !! rowNum

getCol :: Gameboard -> Int -> Column
getCol gb colNum = map (\row -> row !! colNum) gb

getCell :: Gameboard -> Position -> Cell
getCell gb (Posn row col) = gb !! row !! col

getBox :: Gameboard -> Int -> Int -> Box
getBox gb boxRow boxCol = [[getCell gb (Posn row col) |
                                col <- (take 3 [(3*(div boxCol 3))..])] |
                                row <- (take 3 [(3*(div boxRow 3))..])]

getBoxFreeSet :: Box -> FreeSet
getBoxFreeSet box = sodokuSetComplement boxVals
    where boxVals = [val | (Answer val) <- (concat box)]

getVecFreeSet :: [Cell] -> FreeSet
getVecFreeSet vec = sodokuSetComplement vecVals
    where vecVals = [val | (Answer val) <- vec]

sodokuSetComplement :: UsedSet -> FreeSet
sodokuSetComplement usedSet = setComplement [1..9] usedSet

setUnion :: (Eq a) => [a] -> [a] -> [a]
setUnion set1 set2 = rmdups (set1 ++ set2)

setIntersect :: (Eq a) => [a] -> [a] -> [a]
setIntersect set1 set2 = [val | val <- set1, elem val set2]

setComplement :: (Eq a) => [a] -> [a] -> [a]
setComplement universe set = [uniVal | uniVal <- universe,
                                       not (elem uniVal set)]
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = (x:(rmdups (filter (/= x) xs)))
