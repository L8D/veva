{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.Queries.Status
    ( module Veva.Types.Status
    , findById
    , listRange
    , listByUserId
    ) where

import Data.Functor   ((<$>))
import Hasql.Postgres (Postgres)
import Data.Time      (UTCTime)
import Hasql          (Tx, maybeEx, listEx, stmt)

import Veva.Types.Status

import qualified Veva.Types.User   as User

findById :: Id -> forall s. Tx Postgres s (Maybe Status)
findById sid = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, author_id, content, created_at, updated_at
        FROM statuses
        WHERE id = ?
    |] sid

listRange :: Int -> Int -> forall s. Tx Postgres s [Status]
listRange o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, author_id, content, created_at, updated_at
        FROM statuses
        OFFSET ?
        LIMIT ?
    |] o l

listByUserId :: User.Id -> Int -> Int ->
                        forall s. Tx Postgres s [Status]
listByUserId uid o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, author_id, content, created_at, updated_at
        FROM statuses
        WHERE user_id = ?
        OFFSET ?
        LIMIT ?
    |] uid o l

fromRow :: (Id, Author, Content, UTCTime, UTCTime) -> Status
fromRow (sid, own, cnt, cat, uat) = Status sid own cnt cat uat
