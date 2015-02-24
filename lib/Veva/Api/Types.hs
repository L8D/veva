{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Veva.Api.Types
    ( VevaApi(..)
    , query
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (lift)
import Happstack.Server    (ServerPartT)
import Hasql.Postgres      (Postgres)
import Hasql               (Session, Tx, tx, TxIsolationLevel(ReadCommitted))

newtype VevaApi a = VevaApi
    { unVevaApi :: ServerPartT (Session Postgres IO) a }
    deriving ( Applicative
             , Functor
             , Monad
             )

query :: (forall s. Tx Postgres s a) -> VevaApi a
query x = VevaApi $ lift $ tx (Just (ReadCommitted, Just True)) x
