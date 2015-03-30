{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , DeriveDataTypeable
  , OverloadedStrings
  #-}

module Veva.Types.NewUser
    ( NewUser(..)
    , Email(..)
    , Password(..)
    ) where

import Data.JSON.Schema
import Hasql.Postgres   (Postgres)
import Data.Typeable    (Typeable)
import Hasql.Backend    (CxValue(..))
import GHC.Generics     (Generic)
import Data.Aeson       (FromJSON, ToJSON)
import Data.Text        (Text)

import qualified Veva.Types.User as User

newtype Email = Email { unEmail :: User.Email }
    deriving ( Eq
             , Show
             , Generic
             , Typeable
             , FromJSON
             , ToJSON
             , JSONSchema
             , CxValue Postgres
             )

newtype Password = Password { unPassword :: Text }
    deriving ( Eq
             , Show
             , Generic
             , Typeable
             , FromJSON
             , ToJSON
             , JSONSchema
             , CxValue Postgres
             )

data NewUser = NewUser
    { email    :: Email
    , password :: Password
    } deriving (Eq, Show, Generic, Typeable)

instance FromJSON   NewUser
instance ToJSON     NewUser
instance JSONSchema NewUser where schema = gSchema
