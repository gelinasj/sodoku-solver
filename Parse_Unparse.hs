module Parse_Unparse
( parseBoard
, unparseBoard
) where
import Sodoku_Lang

parseCell :: Char -> Cell
parseCell '_' = Empty
parseCell char
    | isDigit = Value (read [char])
    where isDigit = elem char ['1'..'9']

parseBoard :: String -> Gameboard
parseBoard gameboardStr = map (\row -> take 9 $ drop (row*9) cells) [0..8]
    where cells = [parseCell char |
                        char <- gameboardStr,
                        elem char ('_':['1'..'9'])]

unparseBoard :: Gameboard -> String
unparseBoard gb = unparseBoardWrap (getNext gb) boardTemplateString []

unparseBoardWrap ::  Maybe (Cell, Gameboard) -> [Char] -> [Char] -> [Char]
unparseBoardWrap Nothing restStr strAcc = (reverse strAcc) ++ restStr
unparseBoardWrap (Just (cell,itr)) ('@':restStr) strAcc =
    unparseBoardWrap (getNext itr) restStr ((unparseCell cell):strAcc)
unparseBoardWrap itr (char:restStr) strAcc =
    unparseBoardWrap itr restStr (char:strAcc)

getNext :: Gameboard -> Maybe (Cell, Gameboard)
getNext [] = Nothing
getNext ([]:restRows) = getNext restRows
getNext (row:restRows) = Just (head row, (tail row):restRows)

unparseCell :: Cell -> Char
unparseCell Empty = '_'
unparseCell (Value num) = head (show num)


boardTemplateString = "\
\-------------------------------------------\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|-----------------------------------------|\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|-----------------------------------------|\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\|  @   @   @  |  @   @   @  |  @   @   @  |\n\
\-------------------------------------------"
