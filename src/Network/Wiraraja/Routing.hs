module Network.Wiraraja.Routing where

import Control.Applicative (Alternative, (<|>), empty)

import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Map             as M

import qualified Network.HTTP.Types   as H

data RoutePart
    = Path !T.Text
    | Query (M.Map T.Text T.Text)

data Route = Route !H.Method [B.ByteString] [RoutePart]

newtype Match a = Match (Route -> Maybe (Route, a))

end :: Match ()
end = Match $ \(Route m t rp) -> case rp of
    Query _ : [] -> Just (Route m t [], ())
    []           -> Just (Route m t [], ())
    _            -> Nothing

method :: H.Method -> Match ()
method n = Match $ \(Route m t rp) ->
    if n == m then Just (Route m t rp, ()) else Nothing

runMatch :: Match a -> Route -> Maybe a
runMatch (Match k) r = maybe Nothing (Just . snd) (k r)

instance Functor Match where
    fmap f (Match k) = Match $ \x -> maybe Nothing (Just . fmap f) (k x)

instance Applicative Match where
    pure x = Match (\r -> Just (r, x))
    Match ff <*> Match fa = Match $ \x -> case ff x of
        Nothing     -> Nothing
        Just (y, f) -> case fa y of
            Nothing     -> Nothing
            Just (r, z) -> Just (r, f z)

instance Alternative Match where
    empty = Match $ \_ -> Nothing
    Match fa <|> Match fb = Match $ \r -> case fa r of
        Nothing -> fb r
        Just x  -> Just x
