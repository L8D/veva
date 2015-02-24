module Veva.Api
    ( module Veva.Api.Types
    , api
    ) where

import qualified Veva.User.Resource as User
import           Veva.Api.Types

import Rest.Api

api :: Api VevaApi
api = [(mkVersion 1 0 0, Some1 router)]

router :: Router VevaApi VevaApi
router =
    root -/ user
    where user = route User.resource
