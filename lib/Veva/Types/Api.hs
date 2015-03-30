{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Veva.Types.Api
    ( VevaApi(..)
    , query
    ) where

import Control.Applicative (Applicative, Alternative(..))
import Control.Monad.Trans (lift)
import Happstack.Server    (ServerPartT)
import Hasql.Postgres      (Postgres)
import Control.Monad       (MonadPlus(..))
import Hasql               (Session, Tx, tx, TxIsolationLevel(ReadCommitted))

newtype VevaApi a = VevaApi
    { unVevaApi :: ServerPartT (Session Postgres IO) a }
    deriving ( Applicative
             , Alternative
             , MonadPlus
             , Functor
             , Monad
             )

instance MonadPlus m => MonadPlus (Session c m) where
    mzero = lift mzero
    mplus x _ = x -- totally broken

instance MonadPlus m => Alternative (Session c m) where
    empty = lift mzero
    x <|> _ = x -- so much cringe :(

query :: (forall s. Tx Postgres s a) -> VevaApi a
query x = VevaApi $ lift $ tx (Just (ReadCommitted, Just True)) x
