{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)

import qualified Database.Persist.Sqlite  as Sqlite
import qualified Model
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import qualified Api
import           Diener                   (LogEnv, LogLevel (..), runDienerT,
                                           withLogger)

import           Types                    (Diener, DienerEnv (..), DienerErr)

main :: IO ()
main = runInDienerEnv $ \env -> do
  let port = 3000 :: Int
  putStrLn $ "Listening on port " ++ show port ++ " ..."
  Warp.run port $ serve Api.api $ Api.server env

runInDienerEnv :: (LogEnv DienerEnv -> IO a) -> IO a
runInDienerEnv action =
  withLogger "server.log" LevelDebug $ \logFn ->
    Sqlite.withSqlitePool "sqlite3.db" 1 $ \pool -> do
      Sqlite.runSqlPool (Sqlite.runMigration Model.migrateAll) pool
      let env = (logFn, DienerEnv pool)
      liftIO $ action env

testDienerAction :: Diener a -> IO (Either DienerErr a)
testDienerAction action = runInDienerEnv $ \env -> runDienerT env action

