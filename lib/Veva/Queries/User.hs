{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.Queries.User
    ( module Veva.Types.User
    , findById
    , findByEmail
    , listRange
    ) where

import Data.Functor   ((<$>))
import Hasql.Postgres (Postgres)
import Data.Time      (UTCTime)
import Hasql          (Tx, maybeEx, listEx, stmt)

import Veva.Types.User

findById :: Id -> forall s. Tx Postgres s (Maybe User)
findById uid = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        WHERE id = ?
    |] uid

findByEmail :: Email -> forall s. Tx Postgres s (Maybe User)
findByEmail adr = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        WHERE email = ?
    |] adr

listRange :: Int -> Int -> forall s. Tx Postgres s [User]
listRange o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        OFFSET ?
        LIMIT ?
    |] o l

fromRow :: (Id, Email, UTCTime, UTCTime) -> User
fromRow (i, e, c, u) = User i e c u
