{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Backend.Test.UserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import FileCollector.Backend.Test.Util
import FileCollector.Common.Types
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method

spec :: Spec
spec = with defaultApp $
    describe "ApiGetUser" $ do
      it "gets info of current user" $
        request methodGet "/api/user/zelinf" [authHeaderZelinf] ""
          `shouldRespondWith'` matchJSON (User "zelinf" RoleUploader)
      it "rejects with 404 if a non-admin is fetching another user's info" $
        request methodGet "/api/user/lagrand" [authHeaderZelinf] ""
          `shouldRespondWith'` 404
      -- it "gets info of any use for admin" $
      --   pending

authHeaderZelinf :: Header
authHeaderZelinf = authHeader "zelinf" "abcdef"
