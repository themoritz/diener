module Types where

import           Control.Exception (Exception)
import           Database.Gerippe  (ConnectionPool)
import           Diener            (MakeDiener, MakeDienerT)

type Diener = MakeDiener DienerErr DienerEnv
type DienerT = MakeDienerT DienerErr DienerEnv

data DienerEnv = DienerEnv
  { db :: ConnectionPool
  }

data DienerErr
  = ErrNotFound
  | ErrDatabaseQuery
  deriving (Show)

instance Exception DienerErr
