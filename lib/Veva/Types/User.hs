{-# LANGUAGE
    DeriveGeneric
  , DeriveDataTypeable
  #-}

module Veva.Types.User
    ( User(..)
    , module Veva.Types.User.Id
    , module Veva.Types.User.Email
    ) where

import Data.JSON.Schema
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
import Data.Aeson          (FromJSON(..), ToJSON(..))
import Data.Time           (UTCTime)

import Veva.Types.User.Id
import Veva.Types.User.Email

data User = User
    { id         :: Id
    , email      :: Email
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

instance FromJSON   User
instance ToJSON     User
instance JSONSchema User where schema = gSchema
