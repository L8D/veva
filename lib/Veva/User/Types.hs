{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Veva.User.Types
    ( User(..)
    , Id(..)
    , Email(..)
    ) where

import Text.Email.Validate (EmailAddress, emailAddress, toByteString, validate)
import Data.Text.Encoding  (encodeUtf8, decodeUtf8)
import Data.Aeson.Types    (typeMismatch)
import Data.JSON.Schema
import Data.UUID.Aeson     ()
import Hasql.Postgres      (Postgres)
import Hasql.Backend       (CxValue(..))
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
import Data.Functor        ((<$>))
import Rest.ShowUrl        (ShowUrl)
import Data.Aeson          (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Time           (UTCTime)
import Data.Text           (pack)
import Data.UUID           (UUID)
import Rest.Info           (Info(..))

newtype Id = Id { unId :: UUID }
    deriving ( Eq
             , Show
             , FromJSON
             , ToJSON
             , Generic
             , CxValue Postgres
             , ShowUrl
             , Typeable
             )

instance Info Id where
    describe _ = "userId"

instance Read Id where
    readsPrec d r = map f (readsPrec d r) where f (i, s) = (Id i, s)

instance JSONSchema Id where
    schema _ = Value LengthBound
        { lowerLength = Just 36
        , upperLength = Just 36
        }

newtype Email = Email { unEmail :: EmailAddress }
    deriving (Eq, Show, Generic, Typeable)

instance Read Email where
    readsPrec d r = map f (readsPrec d r) where f (i, s) = (Email i, s)

instance Info Email where
    describe _ = "email"

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

instance JSONSchema Email where schema _ = Value unboundedLength

data User = User
    { id         :: Id
    , email      :: Email
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

instance FromJSON   User
instance ToJSON     User
instance JSONSchema User where schema = gSchema
