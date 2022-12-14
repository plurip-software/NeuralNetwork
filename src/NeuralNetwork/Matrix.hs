module NeuralNetwork.Matrix
  ( Matrix(..)
  , Dimensions(..)
  , empty
  , getRow
  ) where

import           Prelude

newtype Matrix =
  Matrix [Row]
  deriving (Show)

newtype Row =
  Row Cells
  deriving (Show)

newtype Column =
  Column Cells
  deriving (Show)

type Cells = [Cell]

data Cell =
  Cell Location Value
  deriving (Show)

data Location =
  Location Row' Col
  deriving (Show)

type Row' = Int

type Col = Int

type Value = Int

data Dimensions =
  Dimensions Rows Cols
  deriving (Show)

type Rows = Int

type Cols = Int

empty :: Dimensions -> Matrix
empty (Dimensions rows cols) =
  Matrix $
  map
    (\row -> Row $ map (\col -> Cell (Location row col) 0) [0 .. (cols - 1)])
    [0 .. (rows - 1)]

getRow :: Row' -> Matrix -> Maybe Row
getRow row (Matrix rows) =
  if row < length rows
    then Just $ rows !! row
    else Nothing

getCol :: Col -> Matrix -> Maybe Column
getCol col (Matrix rows) = 
  if col <= length $ head 
  map (!! col) rows
  
-- transpose :: Matrix -> Matrix
-- transpose
