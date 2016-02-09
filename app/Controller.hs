{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Controller
  ( blogposts
  , blogpostAdd
  , blogpostView
  , personAddJohn
  ) where

import           Control.Exception.Lifted (SomeException (..), catch)
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text

import           Database.Gerippe         (ConnectionPool, SqlPersistT,
                                           entityVal, fromSqlKey, get, getAll,
                                           insert, insert_, runSqlPool)
import           Diener                   (MonadError, MonadLogger, asks,
                                           logError, logInfo, throwError)

import           Model                    (BlogPost (..), BlogPostId, Person (..))
import           Types                    (Diener, DienerEnv (db),
                                           DienerErr (..))

runDb :: (MonadLogger Diener, MonadError e Diener)
      => ConnectionPool -> e -> SqlPersistT Diener a -> Diener a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (Text.pack . show) e
    throwError err

runQuery :: SqlPersistT Diener a -> Diener a
runQuery query = do
  pool <- asks db
  runDb pool ErrDatabaseQuery query

blogposts :: Diener [BlogPost]
blogposts = do
  $logInfo "Request: view all blogposts"
  entities <- runQuery getAll
  pure $ map entityVal entities

blogpostAdd :: BlogPost -> Diener BlogPost
blogpostAdd blogpost = do
  runQuery $ insert blogpost
  pure blogpost

blogpostView :: BlogPostId -> Diener BlogPost
blogpostView blogpostId = do
  $logInfo $ "Request view blogpost by id: " <> (Text.pack . show . fromSqlKey) blogpostId
  runQuery (get blogpostId)
    >>= maybe (throwError ErrNotFound) pure

personAddJohn :: Diener Person
personAddJohn = do
  $logInfo "Adding John"
  let john = Person "John" (Just 42)
  johnId <- runQuery $ insert john
  $logInfo "Adding John's blog post, about his life"
  runQuery $ insert_ (BlogPost "about life" johnId)
  pure john