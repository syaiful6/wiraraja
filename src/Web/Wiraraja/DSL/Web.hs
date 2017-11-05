{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Wiraraja.DSL.Web where

import           Control.Applicative (Applicative(..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State  (StateT)

import           Data.Text (Text)
import           Data.Monoid (Endo)

import           Blaze.ByteString.Builder (Builder)
import qualified Network.Wai        as W
import qualified Network.HTTP.Types as H

import           Web.Wiraraja.Free.Trans  (FreeT)
import           Web.Wiraraja.Session     (SessionMap, Header)
import           Web.Wiraraja.Coroutine   (Source)

data Request = Request
    { reqWaiRequest :: !W.Request
    , reqGetParams  :: ![(Text, Text)]
    , reqCookies    :: ![(Text, Text)]
    , reqLangs      :: ![Text]
    , reqToken      :: !Text
    , reqSession    :: !SessionMap
    , reqAccept     :: ![Text]
    }

data Response
    = ResponseApp !W.Application
    | WaiResponse !W.Response
    | Response !H.Status ![Header] !ContentType !Content !SessionMap

type ContentType = Text

data ContentType
    = ContentBuilder !Builder !(Maybe Int)
    | ContentProducer !(Source (Flush Builder) IO)
    | ContentFile !FilePath !(Maybe W.FilePart)

data Flush a = Chunk a | Flush

instance Functor Flush where
    fmap f (Flush a) = Flush (f a)
    fmap _ Chunk     = Chunk

data ActionState = ActionState
    { asHeaders :: Endo [Header]
    , asSession :: SessionMap
    }

data ActionEnv = ActionEnv
    { aeReq    :: !Request
    , aeParams :: ![(Text, Text)]
    }

newtype HandlerT m a = HandlerT { unHandlerT :: ErrorT HTTPError (ReaderT ActionEnv (StateT ActionState m)) a }
    deriving (Functor, Applicative, MonadIO)

instance Monad m => Monad (HandlerT m) where
    return = HandlerT . return
    HandlerT m >>= k = HandlerT (m >>= unHandlerT . k)
