module Main where
import System.IO
import Parse_Unparse
import Sodoku_Lang
import Solver

main :: IO ()
main = do
    inputString <- readFile "UnsolvedGame.txt"
    putStrLn inputString
    let inputBoard = parseBoard inputString
        outputBoard = solve inputBoard
        outputString = unparseBoard outputBoard
    writeFile "SolvedGame.txt" outputString
