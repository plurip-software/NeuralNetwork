module NeuralNetwork
  ( NeuralNetwork(..)
  ) where

import Prelude
import           NeuralNetwork.Matrix

data NeuralNetwork =
  NeuralNetwork Input Hidden Output
  deriving Show

type InputNodes = Int

type HiddenNodes = Int

type OutputNodes = Int

newtype Layer =
  Layer (Matrix Int) deriving Show

newtype Input =
  Input Layer deriving Show

newtype Hidden =
  Hidden [Layer] deriving Show

newtype Output =
  Output Layer deriving Show
