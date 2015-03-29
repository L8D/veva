{-# LANGUAGE DeriveDataTypeable #-}

module Veva.Status.Resource
    ( resource
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans  (lift)
import Data.Typeable        (Typeable)

import           Veva.Api.Types      (VevaApi, query)
import           Veva.Status.Queries
import qualified Veva.Status.Types   as Status
import qualified Veva.User.Types     as User

import           Rest
import           Rest.Info
import           Rest.ShowUrl
import qualified Rest.Resource as R

data Identifier = ById Status.Id
    deriving (Eq, Show, Read, Typeable)

data Listing = All | ByAuthorId User.Id
    deriving (Eq, Show, Read, Typeable)

instance Info Identifier where
    describe _ = "identifier"

instance ShowUrl Identifier where
    showUrl (ById sid)     = showUrl sid

instance Info Listing where
    describe _ = "listing"

instance ShowUrl Listing where
    showUrl All            = "all"
    showUrl (ByAuthorId uid) = show uid

type WithStatus = ReaderT Identifier VevaApi

resource :: Resource VevaApi WithStatus Identifier Listing Void
resource = mkResourceReader
    { R.name   = "statuses"
    , R.schema = withListing All $ named [ ("id", singleRead ById)
                                         , ("user", listingRead ByAuthorId)
                                         ]
    , R.list   = list
    , R.get    = Just get
    }

list :: Listing -> ListHandler VevaApi
list l = mkListing jsonO handler where
    handler :: Range -> ExceptT Reason_ VevaApi [Status.Status]
    handler r = lift $ query (f (offset r) (count r)) where
        f = case l of
            All -> listStatuses
            ByAuthorId uid -> listStatusesByUserId uid

get :: Handler WithStatus
get = mkIdHandler jsonO handler where
    handler :: () -> Identifier -> ExceptT Reason_ WithStatus Status.Status
    handler _ (ById sid) = lift (lift $ query $ findStatusById sid)
        >>= maybe (throwError NotFound) return
