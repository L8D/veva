{-# LANGUAGE DeriveDataTypeable #-}

module Veva.Resources.User
    ( resource
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans  (lift)
import Data.Typeable        (Typeable)

import           Veva.Types.Api    (VevaApi, query)
import qualified Veva.Queries.User as User

import           Rest
import           Rest.Info
import           Rest.ShowUrl
import qualified Rest.Resource as R

data Identifier = ById User.Id | ByEmail User.Email
    deriving (Eq, Show, Read, Typeable)

instance Info Identifier where
    describe _ = "identifier"

instance ShowUrl Identifier where
    showUrl (ById uid)    = showUrl uid
    showUrl (ByEmail adr) = showUrl adr

type WithUser = ReaderT Identifier VevaApi

resource :: Resource VevaApi WithUser Identifier () Void
resource = mkResourceReader
    { R.name   = "users"
    , R.schema = withListing () $ named [ ("id", singleRead ById)
                                        , ("email", singleRead ByEmail)
                                        ]
    , R.list   = const list
    , R.get    = Just get
    }

list :: ListHandler VevaApi
list = mkListing jsonO handler where
    handler :: Range -> ExceptT Reason_ VevaApi [User.User]
    handler r = lift $ query $ User.listRange (offset r) (count r)

get :: Handler WithUser
get = mkIdHandler jsonO handler where
    handler :: () -> Identifier -> ExceptT Reason_ WithUser User.User
    handler _ idnt = lift (lift $ query $ q idnt) `orThrow` NotFound

    q (ById uid)    = User.findById uid
    q (ByEmail adr) = User.findByEmail adr
