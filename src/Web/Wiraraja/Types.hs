module Web.Wiraraja.Types where

import           Control.Arrow       (first)
import           Control.Applicative ((<$>))

import           Data.ByteString    (ByteString)
import           Data.Map           (Map)
import qualified Data.Map as        M
import           Data.Serialize     (Serialize (..), putByteString)
import           Data.Text          (Text)
import qualified Data.Text as       T
import           Data.Time          (UTCTime)

import           Web.Cookie         (SetCookie)
import           Web.Wiraraja.Utils (putTime, getTime)

import qualified Network.Wai as W


type SessionMap = Map Text ByteString

type SaveSession = SessionMap -> IO [Header]

newtype SessionBackend = SessionBackend
    { sbLoadSession :: W.Request -> IO (SessionMap, SaveSession)
    }

data SessionCookie = SessionCookie (Either UTCTime ByteString) ByteString SessionMap
    deriving (Show, Read)

instance Serialize SessionCookie where
    put (SessionCookie a b c) = do
        either putTime putByteString a
        put b
        put (map (first T.unpack) $ M.toList c)

    get = do
        a <- getTime
        b <- get
        c <- map (first T.pack) <$> get
        return $ SessionCookie (Left a) b (M.fromList c)

data Header =
    AddCookie SetCookie
    | DeleteCookie ByteString ByteString
    | Header ByteString ByteString
    deriving (Eq, Show)
