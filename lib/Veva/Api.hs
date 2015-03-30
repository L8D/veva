module Veva.Api
    ( module Veva.Types.Api
    , api
    ) where

import qualified Veva.Resources.User   as User
import qualified Veva.Resources.Status as Status
import           Veva.Types.Api

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
