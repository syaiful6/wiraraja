{-# LANGUAGE OverloadedStrings #-}

module Web.Wiraraja.Routing
  ( Path
  , Route(..)
  , Match(..)
  , end
  , lit
  , bool
  , text
  , method
  , runMatch
  , routeMiddleware
  ) where

import           Control.Applicative (Alternative, (<|>), empty)

import qualified Data.Text as T

import qualified Network.HTTP.Types as H
import           Network.Wai (Request, Application, Middleware, pathInfo
                             ,requestMethod)


type Path = T.Text

data Route = Route !H.Method ![Path]

newtype Match a = Match (Route -> Maybe (Route, a))

-- | Match against an end path piece
end :: Match ()
end = Match $ \(Route m rp) -> case rp of
    []           -> Just (Route m [], ())
    p : []
      | T.null p -> Just (Route m [], ())
    _            -> Nothing
{-# INLINE end #-}

lit :: T.Text -> Match ()
lit !t = parseSegment parse
  where
    parse s
      | s == t    = Just ()
      | otherwise = Nothing
{-# INLINE lit #-}

bool :: Match Bool
bool = parseSegment parse
  where
    parse s
      | s == "true"  = Just True
      | s == "false" = Just False
      | otherwise    = Nothing
{-# INLINE bool #-}

text :: Match T.Text
text = parseSegment Just
{-# INLINE text #-}

method :: H.Method -> Match H.Method
method n = Match $ \(Route m rp) ->
    if n == m then Just (Route m rp, m) else Nothing
{-# INLINE method #-}

parseSegment :: (T.Text -> Maybe a) -> Match a
parseSegment k = Match $ \(Route m rp) -> case rp of
    p : ps    -> (\x -> (Route m ps, x)) <$> k p
    _         -> Nothing
{-# INLINE parseSegment #-}

runMatch :: Match a -> Route -> Maybe a
runMatch (Match k) r = snd <$> k r
{-# INLINE runMatch #-}

buildRoute :: Request -> Route
buildRoute req = Route (requestMethod req) (pathInfo req)
{-# INLINE buildRoute #-}

routeMiddleware :: Match Application -> Middleware
routeMiddleware match app req = case runMatch match (buildRoute req) of
    Just routeApp -> routeApp req
    Nothing       -> app req
{-# INLINE routeMiddleware #-}

instance Functor Match where
    fmap f (Match k) = Match $ \x -> case k x of
        Nothing     -> Nothing
        Just (r, y) -> Just (r, f y)
    {-# INLINE fmap #-}

instance Applicative Match where
    pure x = Match (\r -> Just (r, x))
    {-# INLINE pure #-}

    Match ff <*> Match fa = Match $ \x -> case ff x of
        Nothing     -> Nothing
        Just (y, f) -> case fa y of
            Nothing     -> Nothing
            Just (r, z) -> Just (r, f z)
    {-# INLINE (<*>) #-}

instance Alternative Match where
    empty = Match $ \_ -> Nothing
    {-# INLINE empty #-}

    Match fa <|> Match fb = Match $ \r -> case fa r of
        Nothing -> fb r
        Just x  -> Just x
    {-# INLINE (<|>) #-}
