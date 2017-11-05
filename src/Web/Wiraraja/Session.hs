module Web.Wiraraja.Session where

import           Control.Arrow       (first)
import           Control.Applicative ((<$>))
import           Control.AutoUpdate  (mkAutoUpdate, defaultUpdateSettings, updateFreq, updateAction)
import           Control.DeepSeq (NFData(rnf))
import           Control.Monad (guard)

import           Data.ByteString    (ByteString)
import           Data.Map           (Map)
import qualified Data.Map as        M
import           Data.Serialize     (Serialize (..), runPut, putByteString, decode, encode)
import           Data.Text          (Text)
import qualified Data.Text as       T
import           Data.Time          (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import qualified Web.ClientSession as CS

import           Web.Cookie         (SetCookie)
import           Web.Wiraraja.Utils (putTime, getTime)

import qualified Network.Wai as W


encodeClientSession :: CS.Key
                    -> CS.IV
                    -> ClientSessionDateCache -- ^ current time
                    -> ByteString -- ^ remote host
                    -> SessionMap -- ^ session
                    -> ByteString -- ^ cookie value
encodeClientSession key iv date rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expires rhost session'
  where
    expires = Right (csdcExpiresSerialized date)

decodeClientSession :: CS.Key
                    -> ClientSessionDateCache -- ^ current time
                    -> ByteString -- ^ remote host field
                    -> ByteString -- ^ cookie value
                    -> Maybe SessionMap
decodeClientSession key date rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie (Left expire) rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > csdcNow date
    guard $ rhost' == rhost
    return session'

clientSessionDateCacher :: NominalDiffTime -> IO (IO ClientSessionDateCache, IO ())
clientSessionDateCacher validity = do
    getClientSessionDateCache <- mkAutoUpdate defaultUpdateSettings
        { updateAction = getUpdated
        , updateFreq   = 10000000 -- 10s
        }
    return (getClientSessionDateCache, return ())
  where
    getUpdated = do
        now <- getCurrentTime
        let expires  = validity `addUTCTime` now
            expiresS = runPut (putTime expires)
        return $! ClientSessionDateCache now expires expiresS

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

data ClientSessionDateCache = ClientSessionDateCache
    { csdcNow               :: !UTCTime
    , csdcExpires           :: !UTCTime
    , csdcExpiresSerialized :: !ByteString
    } deriving (Eq, Show)

data Header =
    AddCookie SetCookie
    | DeleteCookie ByteString ByteString
    | Header ByteString ByteString
    deriving (Eq, Show)

instance NFData Header where
    rnf (AddCookie x) = rnf x
    rnf (DeleteCookie x y) = x `seq` y `seq` ()
    rnf (Header x y) = x `seq` y `seq` ()
