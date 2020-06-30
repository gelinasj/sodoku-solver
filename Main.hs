module Main where
import System.IO
import Parse_Unparse
import Sodoku_Lang
import Board_Utils
import Solver

main :: IO ()
main = do
    putStrLn "Starting to solve"
    inputString <- readFile "UnsolvedGame.txt"
    let inputBoard = parseBoard inputString
        outputBoard = solve inputBoard
        correctlySolved = checkSolution outputBoard
        outputString = unparseBoard outputBoard
    do
        putStrLn ("Solution is " ++ (if correctlySolved then "correct" else "incorrect"))
        writeFile "SolvedGame.txt" outputString
