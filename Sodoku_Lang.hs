module Sodoku_Lang
( Cell(..)
, Row
, Column
, Box
, Gameboard
, Position(..)
, FreeSet
, UsedSet
, GameboardIterator
, Strategy
) where

-- | Represents a set of unused values
type FreeSet = [Int]

-- | Represents a set of used values
type UsedSet = [Int]

-- |Represents a cell on the gameboard
data Cell = Options FreeSet
          | Answer Int deriving(Show, Eq)

-- |Represents a row of cells on the gameboard
type Row = [Cell]

-- |Represents a column of cells on the gameboard
type Column = [Cell]

-- |Represents a 3x3 box of cells on the gameboard
type Box = [[Cell]]

-- |Represents a gameboard
type Gameboard = [Row]

-- |Represents a cell position on the gameboard
data Position = Posn { row :: Int
                     , column :: Int
                     } deriving(Show, Eq)

-- |Represents the iteration across the cells of a gameboard
type GameboardIterator = [(Cell, Position)]

-- |Represents a strategy for finding an answer
type Strategy = Gameboard -> Cell -> Position -> Cell
