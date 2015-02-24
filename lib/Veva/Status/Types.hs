{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Veva.Status.Types
    ( Status(..)
    , Id(..)
    , Content(..)
    ) where

import Data.UUID.Aeson     ()
import Hasql.Postgres      (Postgres)
import Hasql.Backend       (CxValue)
import GHC.Generics        (Generic)
import Data.Aeson          (FromJSON, ToJSON)
import Data.Time           (UTCTime)
import Data.Text           (Text)
import Data.UUID           (UUID)

import qualified Veva.User as User

newtype Id = Id { unId :: UUID }
    deriving (FromJSON, ToJSON, Generic, CxValue Postgres)

newtype Content = Content { unContent :: Text }
    deriving (FromJSON, ToJSON, Generic, CxValue Postgres)

data Status = Status
    { id         :: Id
    , user_id    :: User.Id
    , content    :: Content
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving (Generic)
