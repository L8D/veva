{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.Status.Queries
    ( findStatusById
    , listStatuses
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

listStatuses :: Int -> Int -> forall s. Tx Postgres s [Status]
listStatuses o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, user_id, content, created_at, updated_at
        FROM statuses
        OFFSET ?
        LIMIT ?
    |] o l

listStatusesByUserId :: User.Id -> Int -> Int ->
                        forall s. Tx Postgres s [Status]
listStatusesByUserId uid o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, user_id, content, created_at, updated_at
        FROM statuses
        WHERE user_id = ?
        OFFSET ?
        LIMIT ?
    |] uid o l

fromRow :: (Status.Id, Status.Owner, Status.Content, UTCTime, UTCTime)
           -> Status
fromRow (sid, own, cnt, cat, uat) = Status.Status sid own cnt cat uat
