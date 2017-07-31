{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Wiraraja.HTTPError where

import           Blaze.ByteString.Builder (Builder)

import           Data.ByteString (ByteString)

import qualified Network.HTTP.Types as H
import           Network.Wai (Response, responseBuilder)


data HTTPError = HTTPError
    { heHTTPCode      :: Int
    , heReasonPhrase  :: ByteString
    , heBody          :: Builder
    , heHeaders       :: [H.Header]
    }

responseHTTPError :: HTTPError -> Response
responseHTTPError HTTPError{..} = responseBuilder httpStatus heHeaders heBody
  where
    httpStatus = H.mkStatus heHTTPCode heReasonPhrase

-- | HTTP Error Not Found.
httpErr404 :: Builder -> HTTPError
httpErr404 body = HTTPError
    { heHTTPCode     = 404
    , heReasonPhrase = "Not Found"
    , heBody         = body
    , heHeaders      = []
    }
