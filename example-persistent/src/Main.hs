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
import           Diener                   (LogEnv (..), LogLevel (..),
                                           runDienerT, withLogger)
import qualified Diener.Logger            as Logger

import           Types                    (AppError, HandlerEnv (..), HandlerT)

main :: IO ()
main = runInHandlerEnv $ \env -> do
  let port = 3000 :: Int
  putStrLn $ "Listening on port " ++ show port ++ " ..."
  Warp.run port $ serve Api.api $ Api.server env

runInHandlerEnv :: (LogEnv HandlerEnv -> IO a) -> IO a
runInHandlerEnv action = do
  let logSettings = Logger.Settings
        { Logger.filePath = "server.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = False
        }
  withLogger logSettings $ \logFn ->
    Sqlite.withSqlitePool "sqlite3.db" 1 $ \pool -> do
      Sqlite.runSqlPool (Sqlite.runMigration Model.migrateAll) pool
      let env = LogEnv logFn $ HandlerEnv pool
      liftIO $ action env

testHandlerAction :: HandlerT IO a -> IO (Either AppError a)
testHandlerAction action = runInHandlerEnv $ \env -> runDienerT env action
