{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  , DeriveDataTypeable
  , OverloadedStrings
  #-}

module Veva.Status.Types
    ( Status(..)
    , Id(..)
    , Author(..)
    , Content(..)
    ) where

import Data.JSON.Schema
import Data.UUID.Aeson     ()
import Hasql.Postgres      (Postgres)
import Hasql.Backend       (CxValue)
import Data.Typeable       (Typeable)
import Data.Functor        ((<$>))
import GHC.Generics        (Generic)
import Rest.ShowUrl        (ShowUrl)
import Data.Aeson          hiding (Value(..))
import Rest.Info           (Info(..))
import Data.Time           (UTCTime)
import Data.Text           (Text)
import Data.UUID           (UUID)

import qualified Veva.User as User

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
    describe _ = "id"

instance JSONSchema Id where
    schema _ = Value LengthBound
        { lowerLength = Just 36
        , upperLength = Just 36
        }

instance Read Id where
    readsPrec d r = map f (readsPrec d r) where f (i, s) = (Id i, s)

newtype Author = Author { authorId :: User.Id }
    deriving (Eq, Show, Generic, CxValue Postgres, Typeable)

instance FromJSON Author where
    parseJSON = withObject "Author" $ \o -> Author <$> o .: "id"

instance ToJSON Author where
    toJSON (Author uid) = object ["id" .= toJSON uid]

instance JSONSchema Author where
    schema _ = Object [Field "id" True (schema (Proxy :: Proxy User.Id))]

newtype Content = Content { unContent :: Text }
    deriving (Eq, Show, FromJSON, ToJSON, Generic, CxValue Postgres, Typeable)

instance JSONSchema Content where
    schema _ = Value LengthBound
        { lowerLength = Nothing
        , upperLength = Just 144
        }

data Status = Status
    { id         :: Id
    , author     :: Author
    , content    :: Content
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving (Generic, Typeable)

instance FromJSON   Status
instance ToJSON     Status
instance JSONSchema Status where schema = gSchema
