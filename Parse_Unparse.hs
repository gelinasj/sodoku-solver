module Parse_Unparse
( parseBoard
, unparseBoard
) where
import Sodoku_Lang
import Board_Utils

parseCell :: Char -> Cell
parseCell '_' = (Options [1..9])
parseCell char
    | isDigit = Answer (read [char])
    where isDigit = elem char ['1'..'9']

parseBoard :: String -> Gameboard
parseBoard gameboardStr = map (\row -> take 9 $ drop (row*9) cells) [0..8]
    where cells = [parseCell char |
                        char <- gameboardStr,
                        elem char ('_':['1'..'9'])]

unparseBoard :: Gameboard -> String
unparseBoard gb = unparseBoardWrap (initIterator gb) boardTemplateString []

unparseBoardWrap :: GameboardIterator -> [Char] -> [Char] -> [Char]
unparseBoardWrap [] restStr strAcc = (reverse strAcc) ++ restStr
unparseBoardWrap ((cell, _):getNext) ('@':restStr) strAcc =
    unparseBoardWrap getNext restStr ((unparseCell cell):strAcc)
unparseBoardWrap itr (char:restStr) strAcc =
    unparseBoardWrap itr restStr (char:strAcc)

unparseCell :: Cell -> Char
unparseCell (Options _) = '_'
unparseCell (Answer num) = head (show num)

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
