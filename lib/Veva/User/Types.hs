{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  #-}

module Veva.User.Types
    ( User(..)
    , Id(..)
    , Email(..)
    ) where

import Text.Email.Validate (EmailAddress, emailAddress, toByteString, validate)
import Data.Text.Encoding  (encodeUtf8, decodeUtf8)
import Data.Aeson.Types    (typeMismatch)
import Data.UUID.Aeson     ()
import Hasql.Postgres      (Postgres)
import Hasql.Backend       (CxValue(..))
import GHC.Generics        (Generic)
import Data.Functor        ((<$>))
import Data.Aeson          (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Time           (UTCTime)
import Data.Text           (Text, pack)
import Data.UUID           (UUID)

newtype Id = Id { unId :: UUID }
    deriving (FromJSON, ToJSON, Generic, CxValue Postgres)

newtype Email = Email { unEmail :: EmailAddress }
    deriving (Generic)

instance FromJSON Email where
    parseJSON v = Email <$> withText name go v where
        go x = case emailAddress (encodeUtf8 x) of
            Nothing  -> typeMismatch name v
            (Just e) -> return e

        name = "Email"

instance ToJSON Email where
    toJSON = String . decodeUtf8 . toByteString . unEmail

instance CxValue Postgres Email where
    encodeValue = encodeValue . decodeUtf8 . toByteString . unEmail
    decodeValue v = decodeValue v >>= \x -> case validate (encodeUtf8 x) of
        Left l -> Left (pack $ l ++ show x)
        Right r -> Right (Email r)

data User = User
    { id         :: Id
    , email      :: Email
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving (Generic)

instance FromJSON User
instance ToJSON   User
