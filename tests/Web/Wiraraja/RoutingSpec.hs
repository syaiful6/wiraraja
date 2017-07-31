{-# LANGUAGE OverloadedStrings #-}

module Web.Wiraraja.RoutingSpec (spec) where

import Control.Applicative ((<|>))

import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

import qualified Network.HTTP.Types as H

import           Test.Hspec

import           Web.Wiraraja.Routing


data Page = Home
          | About
          | Users H.Method T.Text
          | User H.Method T.Text
          | NotFound
          deriving (Eq, Ord, Show)

matchRoute :: Route -> Page
matchRoute = fromMaybe NotFound . runMatch parser
  where
    parser = Home <$ method H.methodGet <* end
         <|> About <$ (method H.methodGet <* lit "about") <* end
         <|> Users <$> postOrGet <*> (lit "users" *> text) <* end
         <|> User <$> postOrGet <*> (lit "user" *> text) <* end

postOrGet :: Match H.Method
postOrGet = method H.methodGet <|> method H.methodPost

spec :: Spec
spec = do
    describe "Wiraraja Routing" $ do
        it "Can match against home page" $ do
            let res = matchRoute $ Route H.methodGet []
            res `shouldBe` Home

        it "Match against simple route" $ do
            let res = matchRoute $ Route H.methodGet ["about"]
            res `shouldBe` About

        it "Unequal Method should fail the parser" $ do
            let notfound = matchRoute $ Route H.methodPost ["about"]
            notfound `shouldBe` NotFound

        it "Should capture the parameter correctly" $ do
            let users = matchRoute $ Route H.methodPost ["users", "foo"]
            users `shouldBe` Users H.methodPost "foo"

        it "Should match against with url last backslash" $ do
            let users = matchRoute $ Route H.methodPost ["users", "foo", ""]
            users `shouldBe` Users H.methodPost "foo"

        it "Should not match if there are still parts need to match" $ do
            let notfound = matchRoute $ Route H.methodPost ["users", "foo", "", "rest"]
            notfound `shouldBe` NotFound
