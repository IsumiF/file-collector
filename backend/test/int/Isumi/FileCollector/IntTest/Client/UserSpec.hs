module Isumi.FileCollector.IntTest.Client.UserSpec
  ( spec
  ) where

import Isumi.FileCollector.IntTest.Client
import Test.Hspec

spec :: Spec
spec = do
    describe "userRole" $ do
      it "gets correct role for existing user" $
        pending
      it "gets Nothing for non-exist user" $
        pending

