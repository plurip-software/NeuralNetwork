module NeuralNetwork
  ( NeuralNetwork(..)
  ) where

import           NeuralNetwork.Matrix

data NeuralNetwork =
  NeuralNetwork Input Hidden Output

newtype Layer =
  Layer Matrix

newtype Input =
  Input Layer

newtype Hidden =
  Hidden [Layer]

newtype Output =
  Output Layer
