module Veva.Api
    ( module Veva.Api.Types
    , api
    ) where

import qualified Veva.User.Resource   as User
import qualified Veva.Status.Resource as Status
import           Veva.Api.Types

import Rest.Api

api :: Api VevaApi
api = [(mkVersion 1 0 0, Some1 router)]

router :: Router VevaApi VevaApi
router =
    root
        -/ users
        -/ statuses
    where
        users = route User.resource
        statuses = route Status.resource
