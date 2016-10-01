module Types where

import           Database.Gerippe (ConnectionPool)
import           Diener           (DienerT)

type HandlerT = DienerT AppError HandlerEnv

data HandlerEnv = HandlerEnv
  { db :: ConnectionPool
  }

data AppError
  = ErrNotFound
  | ErrDatabaseQuery
  deriving (Show)
