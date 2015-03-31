{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Veva.Types.User.Id
    ( Id(..)
    ) where

import Data.JSON.Schema
import Data.UUID.Aeson     ()
import Hasql.Postgres      (Postgres)
import Hasql.Backend       (CxValue)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
import Rest.ShowUrl        (ShowUrl(..))
import Data.Aeson          (FromJSON, ToJSON)
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
