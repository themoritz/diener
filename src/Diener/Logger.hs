module Diener.Logger
  ( withLogger
  , LogFunction
  ) where

import           Control.Exception.Base (bracket)
import           Control.Monad.Logger
import           Control.Monad.Reader

import qualified Data.ByteString.Char8  as Char8 (putStrLn)
import           Data.Monoid            ((<>))
import           Data.Time

import           System.Log.FastLogger  (LoggerSet, defaultBufSize, fromLogStr,
                                         newFileLoggerSet, pushLogStr,
                                         rmLoggerSet)

type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

withLogger :: FilePath -> LogLevel -> (LogFunction -> LoggingT IO a) -> IO a
withLogger logFile logLevel action =
  bracket (newFileLoggerSet defaultBufSize logFile) rmLoggerSet $ \loggerSet -> do
    let logFn = logMsg loggerSet logLevel
    runLoggingT (action logFn) logFn

logMsg :: LoggerSet -> LogLevel -> LogFunction
logMsg loggerSet maxLogLevel loc _ level msg =
  when (level >= maxLogLevel) $ do
    out <- getOutput
    pushLogStr loggerSet (out <> "\n")
    when (level >= LevelWarn) $ Char8.putStrLn $ fromLogStr out
  where
    getOutput = do
      date <- getDate
      let l  = "[" <> toLogStr (show level) <> "] "
          lc = " @(" <> fileLocStr <> ")"
      return $ date <> l <> toLogStr msg <> lc
    getDate = do
      now <- getCurrentTime
      return $ toLogStr $ formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now <> ": "
    fileLocStr = toLogStr $
             loc_package loc <>
      ":" <> loc_module loc <>
      " " <> loc_filename loc <>
      ":" <> line loc <>
      ":" <> char loc
        where line = show . fst . loc_start
              char = show . snd . loc_start