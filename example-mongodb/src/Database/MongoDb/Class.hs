{-# LANGUAGE TypeFamilies #-}

module Database.MongoDb.Class where

import qualified Data.Bson        as Bson
import qualified Database.MongoDB as Mongo

class ReadOnly m where
  getAll :: FromDocument x => Mongo.Collection -> m [x]

class Insert m where
  insert_ :: ToDocument x => Mongo.Collection -> x -> m ()
  insert :: ToDocument x => Mongo.Collection -> x -> m Bson.Value

class FromDocument x where
  fromDocument :: Bson.Document -> Maybe x

class ToDocument x where
  toDocument :: x -> Bson.Document
