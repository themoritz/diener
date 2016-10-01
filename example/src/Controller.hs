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

import           Model                    (BlogPost (..), BlogPostId,
                                           Person (..))
import           Types                    (AppError (..), HandlerEnv (db),
                                           HandlerT)

runDb :: (MonadLogger (HandlerT IO), MonadError e (HandlerT IO))
      => ConnectionPool -> e -> SqlPersistT (HandlerT IO) a -> HandlerT IO a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (Text.pack . show) e
    throwError err

runQuery :: SqlPersistT (HandlerT IO) a -> HandlerT IO a
runQuery query = do
  pool <- asks db
  runDb pool ErrDatabaseQuery query

blogposts :: HandlerT IO [BlogPost]
blogposts = do
  $logInfo "Request: view all blogposts"
  entities <- runQuery getAll
  pure $ map entityVal entities

blogpostAdd :: BlogPost -> HandlerT IO BlogPost
blogpostAdd blogpost =
  blogpost <$ runQuery (insert blogpost)

blogpostView :: BlogPostId -> HandlerT IO BlogPost
blogpostView blogpostId = do
  $logInfo $ "Request view blogpost by id: " <> (Text.pack . show . fromSqlKey) blogpostId
  runQuery (get blogpostId) >>= maybe
    (throwError ErrNotFound)
    pure

personAddJohn :: HandlerT IO Person
personAddJohn = do
  $logInfo "Adding John"
  let john = Person "John" (Just 42)
  johnId <- runQuery $ insert john
  $logInfo "Adding John's blog post, about his life"
  runQuery $ insert_ (BlogPost "about life" johnId)
  pure john
