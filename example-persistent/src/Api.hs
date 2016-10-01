{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
  ( api
  , server
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, left, right)
import           Servant                    ((:<|>) (..), (:>), (:~>) (Nat),
                                             Capture, Get, JSON, Post,
                                             Proxy (..), Raw, ReqBody,
                                             ServantErr (..), Server, ServerT,
                                             enter, err404, err500,
                                             serveDirectory)

import qualified Controller
import           Diener                     (LogEnv, runDienerT)
import           Model                      (BlogPost, BlogPostId, Person)
import           Types                      (AppError (..), HandlerEnv,
                                             HandlerT)

type RestApi =
       "blogposts" :> Get '[JSON] [BlogPost]
  :<|> "blogpost"  :> "add" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
  :<|> "blogpost"  :> "view" :> Capture "blogpostid" BlogPostId :> Get '[JSON] BlogPost
  :<|> "person"    :> "add" :> "john" :> Get '[JSON] Person

type Api =
       RestApi
  :<|> "home" :> Raw

api :: Proxy Api
api = Proxy

serverT :: ServerT RestApi (HandlerT IO)
serverT = Controller.blogposts
     :<|> Controller.blogpostAdd
     :<|> Controller.blogpostView
     :<|> Controller.personAddJohn

server :: LogEnv HandlerEnv -> Server Api
server env
  = enter dienerToEither serverT
    :<|> serveDirectory "public"
  where
    dienerToEither :: HandlerT IO :~> EitherT ServantErr IO
    dienerToEither = Nat $ \ar ->
      liftIO (runDienerT env ar) >>= \case
        Left err -> left (dienerErrToServantErr err)
        Right a  -> right a

dienerErrToServantErr :: AppError -> ServantErr
dienerErrToServantErr ErrNotFound            = err404 { errBody = "The requested resource could not be found."}
dienerErrToServantErr _                      = err500 { errBody = "Internal server error."}
