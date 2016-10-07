{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Diener
  ( DienerT (..)
  , MonadDiener(..)
  , runDienerT

  , ask
  , asks
  , throwError
  , catchError

  , withLogger
  , LogEnv (LogEnv)
  , LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , MonadLogger
  , monadLoggerLog
  , MonadError
  ) where

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Except        (MonadError, catchError,
                                              throwError)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Logger        (LogLevel (..), MonadLogger (..),
                                              logDebug, logError, logInfo,
                                              logWarn, toLogStr)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import qualified Control.Monad.Reader        as Reader (asks)
import           Control.Monad.RWS           (RWST)
import           Control.Monad.State         (StateT)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Trans.Either  (EitherT)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Control.Monad.Writer        (WriterT)

import           Diener.Logger               (LogFunction, withLogger)

data LogEnv r = LogEnv
  { logFunction :: LogFunction
  , logEnv      :: r
  }

newtype DienerT e r m a
  = DienerT { unDienerT :: ExceptT e (ReaderT (LogEnv r) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError e
             , MonadReader (LogEnv r)
             )

instance MonadIO m => MonadLogger (DienerT e r m) where
  monadLoggerLog loc src lvl msg = do
    f <- Reader.asks logFunction
    liftIO $ f loc src lvl $ toLogStr msg

deriving instance (MonadBase b m) => MonadBase b (DienerT e r m)

instance MonadTrans (DienerT e r) where
  lift = DienerT . lift . lift

instance MonadBaseControl b m => MonadBaseControl b (DienerT e r m) where
  type StM (DienerT e r m) a = ComposeSt (DienerT e r) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadTransControl (DienerT e r) where
  type StT (DienerT e r) a = StT (ExceptT e) (StT (ReaderT r) a)
  liftWith f = DienerT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unDienerT)
  restoreT = DienerT . restoreT . restoreT

runDienerT :: LogEnv r
           -> DienerT e r m a
           -> m (Either e a)
runDienerT env (DienerT m)
  = runReaderT (runExceptT m) env

class (Monad m, Monad io) => MonadDiener e r m io where
  diener :: DienerT e r io a -> m a

instance Monad io => MonadDiener e r (DienerT e r io) io where
  diener = id

instance (MonadDiener e r m io) => MonadDiener e r (ReaderT r m) io where
  diener = lift . diener

instance (MonadDiener e r m io, Monoid w) => MonadDiener e r (WriterT w m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (StateT s m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (EitherT e' m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (ExceptT e' m) io where
  diener = lift . diener

instance (Monoid w, MonadDiener e r m io) => MonadDiener e r (RWST r' w s m) io where
  diener = lift . diener

asks :: Monad m => (r -> a) -> DienerT e r m a
asks f = f <$> Reader.asks logEnv
