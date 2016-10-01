{-# LANGUAGE Rank2Types #-}
-- |

module Types where

import           Control.Applicative    (liftA2)
import           Data.Aeson             (ToJSON (..), Value (String), object,
                                         (.=))
import qualified Data.Bson              as Bson
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Time.Clock        (UTCTime)
import           Database.MongoDB       ((=:))
import qualified Database.MongoDB       as Mongo
import           Text.Parsec            (eof, many1, parse)
import           Text.Parsec.Char       (anyChar, char, noneOf)

import           Database.MongoDb.Class (FromDocument (..), ToDocument (..))

-- technical types

data Env = Env
  { envRunDb :: forall v. Mongo.Action IO v -> IO v
  }

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text

data SubscribeStatus
  = Subscribed
  | Unsubscribed
  deriving (Show, Eq)

instance Bson.Val SubscribeStatus where
  val Subscribed = Bson.String "subscribed"
  val Unsubscribed = Bson.String "unsubscribed"
  cast' (Bson.String "subscribed") = Just Subscribed
  cast' (Bson.String _)            = Just Unsubscribed
  cast' _                          = Nothing

-- application model

newtype Email = Email Text
  deriving (Show, Eq)

instance ToJSON Email where
  toJSON (Email str) = String str

instance Bson.Val Email where
  val (Email str)         = Bson.String str
  cast' (Bson.String str) = mkEmail str
  cast' _                 = Nothing

mkEmail :: Text -> Maybe Email
mkEmail str =
    either (const Nothing) (const $ Just $ Email str) $
        parse email "email" $ Text.unpack str
  where
    email = many1 (noneOf "@")
        <+> pure <$> char '@'
        <+> many1 (noneOf "@.")
        <+> pure <$> char '.'
        <+> many1 anyChar <* eof

    (<+>) = liftA2 mappend
    infixl 3 <+>

data Subscriber = Subscriber
  { subCreated :: UTCTime
  , subEmail   :: Email
  , subStatus  :: SubscribeStatus
  }

instance FromDocument Subscriber where
  fromDocument doc =
    Subscriber
      <$> Bson.lookup "created" doc
      <*> Bson.lookup "email" doc
      <*> Bson.lookup "status" doc

instance ToDocument Subscriber where
  toDocument (Subscriber created email status) =
    [ "created" =: created
    , "email"   =: email
    , "status"  =: status
    ]

instance ToJSON Subscriber where
  toJSON (Subscriber created email status) = object
    [ "created" .= created
    , "email" .= email
    , ("status", case status of
          Subscribed -> "subscribed"
          Unsubscribed -> "unsubscribed"
      )]

data SubscriberAddResult
  = SAROK
  | SARErrorMsg Text

instance ToJSON SubscriberAddResult where
  toJSON SAROK             = object [ ("status", "OK") ]
  toJSON (SARErrorMsg msg) = object
    [ "msg"    .= msg
    , ("status", "error")
    ]
