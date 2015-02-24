{-# LANGUAGE DeriveDataTypeable #-}

module Veva.User.Resource
    ( resource
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans  (lift)
import Data.Typeable        (Typeable)

import           Veva.Api.Types    (VevaApi, query)
import           Veva.User.Queries
import qualified Veva.User.Types   as User

import           Rest
import           Rest.Info
import           Rest.ShowUrl
import qualified Rest.Resource as R

data Identifier = ById User.Id
    deriving (Eq, Show, Read, Typeable)

instance Info Identifier where
    describe _ = "identifier"

instance ShowUrl Identifier where
    showUrl (ById uid)    = showUrl uid

type WithUser = ReaderT Identifier VevaApi

resource :: Resource VevaApi WithUser Identifier () Void
resource = mkResourceReader
    { R.name   = "users"
    , R.schema = withListing () $ named [ ("id", singleRead ById)
                                        ]
    , R.list   = const list
    , R.get    = Just get
    }

list :: ListHandler VevaApi
list = mkListing jsonO handler where
    handler :: Range -> ExceptT Reason_ VevaApi [User.User]
    handler r = lift $ query (listUsers (offset r) (count r))

get :: Handler WithUser
get = mkIdHandler jsonO handler where
    handler :: () -> Identifier -> ExceptT Reason_ WithUser User.User
    handler _ (ById uid) = lift (lift $ query $ findUserById uid)
        >>= maybe (throwError NotFound) return
