{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.Status.Queries
    ( findStatusById
    , listStatusesByUserId
    ) where

import Data.Functor   ((<$>))
import Hasql.Postgres (Postgres)
import Data.Time      (UTCTime)
import Hasql          (Tx, maybeEx, listEx, stmt)

import           Veva.Status (Status)
import qualified Veva.Status as Status
import qualified Veva.User   as User

findStatusById :: Status.Id -> forall s. Tx Postgres s (Maybe Status)
findStatusById sid = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, user_id, content, created_at, updated_at
        FROM statuses
        WHERE id = ?
    |] sid

listStatusesByUserId :: User.Id -> forall s. Tx Postgres s [Status]
listStatusesByUserId uid = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, user_id, content, created_at, updated_at
        FROM statuses
        WHERE user_id = ?
    |] uid

fromRow :: (Status.Id, User.Id, Status.Content, UTCTime, UTCTime) -> Status
fromRow (sid, uid, cnt, cat, uat) = Status.Status sid uid cnt cat uat
