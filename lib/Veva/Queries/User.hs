{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.Queries.User
    ( module Veva.Types.User
    , findById
    , findByEmail
    , listRange
    , new
    ) where

import Data.Functor   ((<$>))
import Hasql.Postgres (Postgres)
import Data.Time      (UTCTime)
import Hasql          (Tx, maybeEx, listEx, stmt)

import Veva.Types.User
import Veva.Types.NewUser (NewUser(NewUser))

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

new :: NewUser -> forall s. Tx Postgres s (Maybe User)
new (NewUser adr pass) = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT insert_user(?, ?)
    |] adr pass

fromRow :: (Id, Email, UTCTime, UTCTime) -> User
fromRow (i, e, c, u) = User i e c u
