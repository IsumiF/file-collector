{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Test.DirectorySpec
  ( spec
  ) where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method
import Test.Hspec
import Test.Hspec.Wai

import FileCollector.Backend.Test.Util
    (authHeader, defaultApp, emptyBody, jsonRequest, matchJSON)
import FileCollector.Common.Types

spec :: Spec
spec = with defaultApp $ do
    describe "ApiGetDirList" $ do
      it "gets dirs that an uploader can upload to" $
        request methodGet "/api/filesystem/dir" [authHeader "zelinf" "abcdef"] ""
          `shouldRespondWith` matchJSON (take 2 dirs)
      it "get own dirs of collector" $
        request methodGet "/api/filesystem/dir" [authHeader "助教-02" "中文密码"] ""
          `shouldRespondWith` matchJSON (drop 2 dirs)
      it "get all dirs, if the user is admin" $
        request methodGet "/api/filesystem/dir" [authHeader "Isumi Fly" "faio31221"] ""
          `shouldRespondWith` matchJSON dirs
      it "respond with 401 if the user credential is incorrect" $
        request methodGet "/api/filesystem/dir" [authHeader "zelinf" "aaaaa"] ""
        `shouldRespondWith` 401
    describe "ApiGetDir" $ do
      it "returns the dir if uploader has permission" $
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeader "zelinf" "abcdef"] emptyBody
          `shouldRespondWith` matchJSON (head dirs)
      it "returns 404 if uploader has no permission" $
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程2-大作业"
          [authHeader "zelinf" "abcdef"] emptyBody
          `shouldRespondWith` 404
      it "returns the dir if the collector owns it" $
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeader "TA-01" "abcdef"] emptyBody
          `shouldRespondWith` matchJSON (head dirs)
      it "returns 404 if the collector doesn't own the dir" $
        jsonRequest methodGet "/api/filesystem/dir/助教-02/课程2-大作业"
          [authHeader "TA-01" "abcdef"] emptyBody
          `shouldRespondWith` 404
    describe "ApiPutDir" $ do
      it "updates the dir's basic properties" $ do
        let newDir = Directory "课程1-作业1" "TA-01" Nothing [ RuleMaxFiles 1 ]
        jsonRequest methodPut "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeader "TA-01" "abcdef"] newDir
          `shouldRespondWith` 200
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeader "TA-01" "abcdef"] emptyBody
          `shouldRespondWith` matchJSON newDir
      it "can be used to transfer ownership" $ do
        let newDir = set directory_ownerName "助教-02" (head dirs)
        jsonRequest methodPut "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeader "TA-01" "abcdef"] newDir
          `shouldRespondWith` 200
        jsonRequest methodGet "/api/filesystem/dir/助教-02/课程1-作业1"
          [authHeader "助教-02" "中文密码"] emptyBody
          `shouldRespondWith` matchJSON newDir
    describe "ApiDeleteDir" $ do
      it "deletes the dir if a collector owns it" $ do
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程1-作业2"
          [authHeader "zelinf" "abcdef"] emptyBody
          `shouldRespondWith` 200
        jsonRequest methodDelete "/api/filesystem/dir/TA-01/课程1-作业2"
          [authHeaderTA1] emptyBody
          `shouldRespondWith` 200
        jsonRequest methodGet "/api/filesystem/dir/TA-01/课程1-作业2"
          [authHeader "zelinf" "abcdef"] emptyBody
          `shouldRespondWith` 404
      it "fails to delete the directory if the collector doesn't own it" $
        jsonRequest methodDelete "/api/filesystem/dir/TA-01/课程1-作业1"
          [authHeaderTA2] emptyBody
          `shouldRespondWith` 404
    describe "ApiDirUploaders.Get" $ do
      it "returns uploaders of a directory" $
        jsonRequest methodGet (baseUrl <> "TA-01/课程1-作业1/uploaders")
          [authHeaderTA1] emptyBody
          `shouldRespondWith` matchJSON (fmap UserName ["zelinf", "同学C"])
      it "rejects with 404 if the collector is not the owner of the dir" $
        jsonRequest methodGet (baseUrl <> "TA-01/课程1-作业1/uploaders")
          [authHeaderTA2] emptyBody
          `shouldRespondWith` 404
    describe "ApiDirUploaders.Put" $
      it "updates the uploader list of a directory" $ do
        jsonRequest methodPut (baseUrl <> "TA-01/课程1-作业1/uploaders")
          [authHeaderTA1] (fmap UserName ["zelinf", "lagrand"])
          `shouldRespondWith` 200
        jsonRequest methodGet (baseUrl <> "TA-01/课程1-作业1/uploaders")
          [authHeaderTA1] emptyBody
          `shouldRespondWith` matchJSON (fmap UserName ["zelinf", "lagrand"])
    describe "ApiGetDirContent" $
      it "returns the uploaded files of an uploader" $
        jsonRequest methodGet (baseUrl <> "TA-01/课程1-作业1/file")
          [authHeaderZelinf] emptyBody
          `shouldRespondWith` matchJSON ([] :: [Text])
  where
    baseUrl = "/api/filesystem/dir/"

dirs :: [Directory]
dirs = [ dir1, dir2, dir3 ]
  where
    dir1 = Directory "课程1-作业1" "TA-01"
      (Just $ UTCTime (fromGregorian 2019 5 8) 0)
      lesson1Rules
    dir2 = Directory "课程1-作业2" "TA-01"
      (Just $ UTCTime (fromGregorian 2019 6 8) 0)
      lesson1Rules
    dir3 = Directory "课程2-大作业" "助教-02"
      Nothing
      lesson2Rules
    lesson1Rules =
      [ RuleMaxFiles 1
      , RuleMaxFileSize (64 * 1024 * 1024)
      , RuleFileNameFormat "[[:digit:]]{8}-.+\\.[[:alnum:]\\.]+"
      ]
    lesson2Rules =
      [ RuleMaxFiles 2
      , RuleMaxFileSize (30 * 1024 * 1024)
      ]

authHeaderTA1 :: Header
authHeaderTA1 = authHeader "TA-01" "abcdef"

authHeaderTA2 :: Header
authHeaderTA2 = authHeader "助教-02" "中文密码"

authHeaderZelinf :: Header
authHeaderZelinf = authHeader "zelinf" "abcdef"
