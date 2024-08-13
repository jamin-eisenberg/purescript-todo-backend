module Model.Todo
  ( Todo(..)
  ) where

import Data.Maybe as Maybe

type Todo =
  { id :: Int
  , title :: String
  , ord :: Maybe.Maybe Int
  , completed :: Boolean
  }