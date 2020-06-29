module Sodoku_Lang
( Cell(..)
, Row
, Gameboard
, Position(..)
, Solution
, Strategy
) where

-- |Represents a cell on the gameboard
data Cell = Empty
          | Value Integer deriving(Show)

-- |Represents a row on the gameboard
type Row = [Cell]

-- |Represents a gameboard
type Gameboard = [Row]

-- |Represents a cell position on the gameboard
data Position = Posn { row :: Integer
                     , column :: Integer
                     }

-- |Represents a known cell Value
type Solution = Integer

-- |Represents a strategy for solving the solution for a particular cell
type Strategy = Gameboard -> Position -> Maybe Solution
