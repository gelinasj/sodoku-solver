module Main where
import System.IO
import Parse_Unparse
import Sodoku_Lang
import Solver

main :: IO ()
main = do
    putStrLn "Starting to solve"
    inputString <- readFile "UnsolvedGame.txt"
    let inputBoard = parseBoard inputString
        outputBoard = solve inputBoard
        outputString = unparseBoard outputBoard
    writeFile "SolvedGame.txt" outputString
