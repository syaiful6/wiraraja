{-# LANGUAGE OverloadedStrings #-}

module Web.Wiraraja.Config where

import           Control.Exception (throwIO)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString
                                             ,createPostgresqlPool)

import           Network.Wai (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           System.Environment (lookupEnv)

import           Web.Wiraraja.HTTPError (HTTPError)


-- | The config for our application
data Config = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment }

type AppM = ReaderT Config (ExceptT HTTPError IO)

-- | The Environment of our application.
data Environment = Development
                 | Test
                 | Production

-- | get logger middleware depending the application's environment
envLogger :: Environment -> Middleware
envLogger Development = logStdoutDev
envLogger Production  = logStdout
envLogger Test        = id

-- | make a connection pool
makeConnPool
    :: Environment -> IO ConnectionPool
makeConnPool Test        =
    runNoLoggingT $ createPostgresqlPool (connStr "test") (envPool Test)
makeConnPool Development =
    runStdoutLoggingT $ createPostgresqlPool (connStr "") (envPool Development)
makeConnPool Production  = do
    -- If we don't have a correct database configuration then we can't handle that
    -- so we will just throw an exception
    pool <- runMaybeT makeConnPoolEnv
    case pool of
        Nothing -> throwIO (userError "Database Configuration not present in environment.")
        Just a  -> return a

makeConnPoolEnv :: MaybeT IO ConnectionPool
makeConnPoolEnv = do
    envList <- traverse (MaybeT . lookupEnv) envs
    let prodStr = mconcat . zipWith (<>) keys $ BS.pack <$> envList
    runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
  where
    keys = [ "host="
           , "port="
           , "user="
           , "password="
           , "dbname=" ]
    envs = [ "PGHOST"
           , "PGPORT"
           , "PGUSER"
           , "PGPASS"
           , "PGDATABASE" ]

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=wiraraja" <> sfx <> " user=wiraraja password=secret port=5432"
