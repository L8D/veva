module Main
    ( main
    ) where

import Happstack.Server.Internal.Monads
import Data.ByteString.Char8 (pack)
import System.Environment    (getEnv)
import Control.Concurrent    (forkIO, killThread)
import Happstack.Server
import Hasql.Postgres
import Data.Functor          ((<$>))
import Hasql

import Veva.Api

import Rest.Driver.Happstack (apiToHandler')

handle :: Pool Postgres -> ServerPartT IO Response
handle p = mapServerPartT' run (apiToHandler' unVevaApi api) where
    run r x = session p x >>= go where
        go (Left e) = ununWebT $ runServerPartT (simpleErrorHandler (show e)) r
        go (Right v) = return v

main :: IO ()
main = do
    postgresSettings <- StringSettings . pack <$> getEnv "DATABASE_URL"

    settings <- maybe (fail "improper settings") return (poolSettings 6 30)

    pool <- acquirePool postgresSettings settings

    tid <- forkIO $ simpleHTTP nullConf (handle pool)

    waitForTermination
    releasePool pool
    killThread tid
