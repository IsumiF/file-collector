{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.FileCollector.IntTest.Client.UserSpec
  ( spec
  ) where

import Test.Hspec

import Isumi.FileCollector.IntTest.Client
import Isumi.FileCollector.IntTest.DbGen.Simple (genDbSimple)
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)

spec :: Spec
spec =
    beforeAll ((,) <$> startTempServer genDbSimple
                   <*> newManager defaultManagerSettings) $
      afterAll (shutdownTempServer . fst) $ do
        describe "userRole" $ do
          it "gets correct role for existing user" $ \(_, m) -> do
            result <-
              runClientM
                (userRole (BasicAuthData "admin" "admin"))
                (clientEnv m)
            result `shouldBe` Right (Just RoleAdmin)

clientEnv :: Manager -> ClientEnv
clientEnv m = mkClientEnv m (BaseUrl Http "localhost" 8081 "")
