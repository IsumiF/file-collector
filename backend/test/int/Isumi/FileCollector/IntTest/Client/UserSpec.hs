{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.FileCollector.IntTest.Client.UserSpec
  ( spec
  ) where

import Test.Hspec

import Isumi.FileCollector.IntTest.Client
import Isumi.FileCollector.IntTest.DbGen.Simple (genDbSimple)

spec :: Spec
spec =
    withTempServer genDbSimple $
      describe "userRole" $ do
        it "gets correct role for existing user" $ runReaderT $ do
          result <- runClientM' (clientUserRole (BasicAuthData "admin" "admin"))
          lift $ result `shouldBe` Right (Just RoleAdmin)
        it "reject with unauthorized for non-exist user" $ runReaderT $ do
          result <- runClientM' (clientUserRole (BasicAuthData "nonexist" "pwd"))
          lift $ servantErrorStatusCode (fromLeft' result)
            `shouldBe` Just 401

