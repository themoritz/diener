{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Diener
  ( MakeDienerT (..)
  , MakeDiener
  , MonadDiener(..)
  , runDienerT

  , ask
  , asks
  , throwError
  , catchError

  , withLogger
  , LogEnv
  , LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , MonadLogger
  , monadLoggerLog
  , MonadError
  ) where

import           Control.Monad.Base
import           Control.Monad.Except        (MonadError, catchError,
                                              throwError)
import           Control.Monad.Logger        (LogLevel (..), MonadLogger (..),
                                              logDebug, logError, logInfo,
                                              logWarn, toLogStr)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Control.Monad.RWS           (RWST)
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except
import           Control.Monad.Writer        (WriterT)

import           Diener.Logger               (LogFunction, withLogger)

type LogEnv r = (LogFunction, r)

newtype MakeDienerT e r m a
  = DienerT { unDienerT :: ExceptT e (ReaderT (LogEnv r) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError e
             , MonadReader (LogEnv r)
             )

instance MonadIO m => MonadLogger (MakeDienerT e r m) where
  monadLoggerLog loc src lvl msg = do
    (f, _) <- ask
    liftIO $ f loc src lvl $ toLogStr msg

deriving instance (MonadBase b m) => MonadBase b (MakeDienerT e r m)

instance MonadTrans (MakeDienerT e r) where
  lift = DienerT . lift . lift

instance MonadBaseControl b m => MonadBaseControl b (MakeDienerT e r m) where
  type StM (MakeDienerT e r m) a = ComposeSt (MakeDienerT e r) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadTransControl (MakeDienerT e r) where
  type StT (MakeDienerT e r) a = StT (ExceptT e) (StT (ReaderT r) a)
  liftWith f = DienerT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unDienerT)
  restoreT = DienerT . restoreT . restoreT

runDienerT :: Monad m
          => LogEnv r
          -> MakeDienerT e r m a
          -> m (Either e a)
runDienerT env (DienerT m)
  = runReaderT (runExceptT m) env

type MakeDiener e r = MakeDienerT e r IO

class Monad m => MonadDiener e r m where
  diener :: MakeDiener e r a -> m a

instance MonadDiener e r (MakeDiener e r) where
  diener = id

instance (MonadDiener e r m) => MonadDiener e r (ReaderT r m) where
  diener = lift . diener

instance (MonadDiener e r m, Monoid w) => MonadDiener e r (WriterT w m) where
  diener = lift . diener

instance (MonadDiener e r m) => MonadDiener e r (StateT s m) where
  diener = lift . diener

instance (MonadDiener e r m) => MonadDiener e r (EitherT e' m) where
  diener = lift . diener

instance (MonadDiener e r m) => MonadDiener e r (ExceptT e' m) where
  diener = lift . diener

instance (Monoid w, MonadDiener e r m) => MonadDiener e r (RWST r' w s m) where
  diener = lift . diener

asks :: (r -> a) -> MakeDiener e r a
asks f = f . snd <$> ask
