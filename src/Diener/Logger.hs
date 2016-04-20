{-# LANGUAGE OverloadedStrings #-}

module Diener.Logger
  ( withLogger
  , LogFunction
  , Settings (..)
  ) where

import           Control.Exception.Base (bracket)
import           Control.Monad.Logger
import           Control.Monad.Reader
import Data.Default (Default, def)
import qualified Data.ByteString.Char8  as Char8 (putStrLn)
import           Data.Monoid            ((<>))
import           Data.Time
import           System.Console.ANSI    (Color (Red), ColorIntensity (Vivid),
                                         ConsoleLayer (Foreground),
                                         SGR (Reset, SetColor), setSGR)

import           System.Log.FastLogger  (LoggerSet, defaultBufSize, fromLogStr,
                                         newFileLoggerSet, pushLogStr,
                                         rmLoggerSet)

type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Settings = Settings
    { filePath :: FilePath
    , logLevel :: LogLevel
    , noConsoleLogging :: Bool
    }

instance Default Settings where
    def = Settings "logfile.log" LevelDebug False

withLogger :: Settings -> (LogFunction -> LoggingT IO a) -> IO a
withLogger (Settings logFile level noConsLogging) action =
  bracket (newFileLoggerSet defaultBufSize logFile) rmLoggerSet $ \loggerSet -> do
    let logFn = logMsg loggerSet level noConsLogging
    runLoggingT (action logFn) logFn

logMsg :: LoggerSet -> LogLevel -> Bool -> LogFunction
logMsg loggerSet maxLogLevel noConsLogging loc _ level msg =
    when (level >= maxLogLevel) $ do
      out <- getOutput
      pushLogStr loggerSet (out <> "\n")
      unless noConsLogging $ do
          when (level == LevelError) $ setSGR [SetColor Foreground Vivid Red]
          Char8.putStrLn $ fromLogStr out
          when (level == LevelError) $ setSGR [Reset]
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
