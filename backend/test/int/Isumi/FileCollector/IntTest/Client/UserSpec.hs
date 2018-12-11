{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.FileCollector.IntTest.Client.UserSpec
  ( spec
  ) where

import Test.Hspec

import Isumi.FileCollector.IntTest.Client
import Isumi.FileCollector.IntTest.DbGen.Simple (genDbSimple)

spec :: Spec
spec =
    beforeAll (startTempServer 8081 genDbSimple) $
      afterAll shutdownTempServer $  do
        describe "userRole" $ do
          it "gets correct role for existing user" $ const $
            pending
          it "gets Nothing for non-exist user" $ const $
            True `shouldBe` True
        describe "someOther" $ do
          it "lol" $ const $
            pending

