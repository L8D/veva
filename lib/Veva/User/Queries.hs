{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.User.Queries
    ( findUserById
    , findUserByEmail
    , listUsers
    ) where

import Data.Functor   ((<$>))
import Hasql.Postgres (Postgres)
import Data.Time      (UTCTime)
import Hasql          (Tx, maybeEx, listEx, stmt)

import           Veva.User (User)
import qualified Veva.User as User

findUserById :: User.Id -> forall s. Tx Postgres s (Maybe User)
findUserById uid = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        WHERE id = ?
    |] uid

findUserByEmail :: User.Email -> forall s. Tx Postgres s (Maybe User)
findUserByEmail adr = fmap fromRow <$> maybeEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        WHERE email = ?
    |] adr

listUsers :: Int -> Int -> forall s. Tx Postgres s [User]
listUsers o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, email, created_at, updated_at
        FROM users
        OFFSET ?
        LIMIT ?
    |] o l

fromRow :: (User.Id, User.Email, UTCTime, UTCTime) -> User
fromRow (i, e, c, u) = User.User i e c u
