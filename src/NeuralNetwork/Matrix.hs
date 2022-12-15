module NeuralNetwork.Matrix
  ( Matrix(..)
  , initMtx
  , randomizeWithInt
  , (><)
  ) where

import           Data.List     (groupBy)
import           Data.Maybe    (mapMaybe)
import           Prelude
import           System.Random (getStdRandom, randomR)

newtype Matrix a =
  Matrix [[a]]
  deriving (Show)

initMtx :: Rows -> Cols -> a -> Matrix a
initMtx rows cols a = Matrix . replicate rows $ replicate cols a

data Range =
  Range Int Int
  deriving (Show)

getRandomInt :: Range -> IO Int
getRandomInt (Range from to) = getStdRandom (randomR (from, to))

randomizeWithInt :: Range -> Matrix Int -> Matrix (IO Int)
randomizeWithInt range (Matrix mtx) =
  Matrix $ map (\row -> map (\_ -> getRandomInt range) row) mtx

type Rows = Int

type Cols = Int

toList :: Matrix a -> [[a]]
toList (Matrix mtx) = mtx

firstRow :: Matrix a -> Maybe [a]
firstRow = getRow 0

countCols :: Matrix a -> Int
countCols mtx =
  case firstRow mtx of
    Just cells -> length cells
    _          -> 0

getRow :: Int -> Matrix a -> Maybe [a]
getRow idx (Matrix mtx) =
  if idx >= 0 && idx < length mtx
    then Just $ mtx !! idx
    else Nothing

getCol :: Int -> Matrix a -> Maybe [a]
getCol idx mtx =
  if idx >= 0 && idx < countCols mtx
    then Just . map (\row -> row !! idx) $ toList mtx
    else Nothing

getCols :: Matrix a -> [[a]]
getCols mtx = mapMaybe (flip getCol mtx) [0 .. (countCols mtx) - 1]

multiply :: (Num a, Eq a) => Matrix a -> Matrix a -> Matrix a
multiply mtx mtx' =
  let mtxRows = toList mtx
      mtxCols = getCols mtx'
      tpls = (,) <$> mtxRows <*> mtxCols
      groupedTpls = groupBy (\(cells, _) (cells', _) -> cells == cells') tpls
      sumCells (cells, cells') = sum $ zipWith (*) cells cells'
   in Matrix $ map (map sumCells) groupedTpls

infixl 5 ><

(><) = multiply
