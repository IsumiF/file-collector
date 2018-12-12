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
          result <- runClientM' (userRole (BasicAuthData "admin" "admin"))
          lift $ result `shouldBe` Right (Just RoleAdmin)

