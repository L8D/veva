{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module Veva.User.Queries
    ( findUserById
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
        SELECT id, email, password, created_at, updated_at
        FROM users
        WHERE id = ?
    |] uid

listUsers :: Int -> Int -> forall s. Tx Postgres s [User]
listUsers o l = fmap fromRow <$> listEx q where q = [stmt|
        SELECT id, email, password, created_at, updated_at
        FROM users
        OFFSET ?
        LIMIT ?
    |] o l

fromRow :: (User.Id, User.Email, UTCTime, UTCTime) -> User
fromRow (i, e, c, u) = User.User i e c u
