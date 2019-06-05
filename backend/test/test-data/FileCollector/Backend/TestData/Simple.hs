{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.TestData.Simple
  ( populateTestData
  , samplePasswords
  , sampleFiles
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List ((!!))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime (..), fromGregorian)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Numeric (readHex)

import           FileCollector.Backend.Database.Types
import           FileCollector.Common.Base.Convertible
import qualified FileCollector.Common.Types as Common

{-| Insert test data into database
-}
populateTestData :: (MonadIO m, MonadReader SqlBackend m, MonadLogger m)
                 => m ()
populateTestData = do
    liftPersist $ insert_ $
      User "Isumi Fly" (snd $ samplePasswords !! 1) (convert Common.RoleAdmin)
    ta1 <- liftPersist $ insert $
      User "TA-01" (snd $ head samplePasswords) (convert Common.RoleCollector)
    ta2 <- liftPersist $ insert $
      User "助教-02" (snd $ samplePasswords !! 2) (convert Common.RoleCollector)
    stu1 <- liftPersist $ insert $
      User "zelinf" (snd $ head samplePasswords) (convert Common.RoleUploader)
    stu2 <- liftPersist $ insert $
      User "lagrand" (snd $ head samplePasswords) (convert Common.RoleUploader)
    stu3 <- liftPersist $ insert $
      User "同学C" (snd $ samplePasswords !! 1) (convert Common.RoleUploader)

    let lesson1Rules :: [UploadRule] = fmap convert
          [ Common.RuleMaxFiles 1
          , Common.RuleMaxFileSize (64 * 1024 * 1024)
          , Common.RuleFileNameFormat "[[:digit:]]{8}-.+\\.[[:alnum:]\\.]+"
          ]

    dir1 <- liftPersist $ insert $ Directory "课程1-作业1" ta1
      (Just $ UTCTime (fromGregorian 2019 5 8) 0)
      lesson1Rules
    dir2 <- liftPersist $ insert $ Directory "课程1-作业2" ta1
      (Just $ UTCTime (fromGregorian 2019 6 8) 0)
      lesson1Rules
    dir3 <- liftPersist $ insert $ Directory "课程2-大作业" ta2
      Nothing
      ( fmap convert
        [ Common.RuleMaxFiles 2
        , Common.RuleMaxFileSize (30 * 1024 * 1024)
        ]
      )

    liftPersist $ insert_ $ CanUploadTo stu1 dir1
    liftPersist $ insert_ $ CanUploadTo stu1 dir2
    liftPersist $ insert_ $ CanUploadTo stu2 dir2
    liftPersist $ insert_ $ CanUploadTo stu2 dir3
    liftPersist $ insert_ $ CanUploadTo stu3 dir1
    liftPersist $ insert_ $ CanUploadTo stu3 dir3

    let emptyHashValue = HashValue $ Common.HashValue Common.HashTypeMD5 ""

    liftPersist $ insert_ $
      File "16337060-zelinf.txt" emptyHashValue stu1 dir1
        (UTCTime (fromGregorian 2019 5 6) 0) "c02ccf54-8600-11e9-bc42-526af7764f64"
    liftPersist $ insert_ $
      File "16337063-lagrand.txt" emptyHashValue stu2 dir1
        (UTCTime (fromGregorian 2019 5 5) (22 * 3600))
        "c02cd1ca-8600-11e9-bc42-526af7764f64"
    liftPersist $ insert_ $
      File "hw3.txt" emptyHashValue stu2 dir3
        (UTCTime (fromGregorian 2019 5 6) 0) "c02cd30a-8600-11e9-bc42-526af7764f64"
    liftPersist $ insert_ $
      File "hw4.txt" emptyHashValue stu3 dir3
        (UTCTime (fromGregorian 2019 5 6) 0) "c02cd436-8600-11e9-bc42-526af7764f64"
    liftPersist $ insert_ $
      File "hw5.txt" emptyHashValue stu3 dir3
        (UTCTime (fromGregorian 2019 5 6) 0) "c02cd562-8600-11e9-bc42-526af7764f64"

-- |Sample passwords, in pairs of plaintext and salted hash
samplePasswords :: [(Text, ByteString)]
samplePasswords = fmap (second (\x -> salt <> bsFromHex x))
    [ ("abcdef", "f2bec0968ad2b03069052e2343ffac08c519082221e46fa5fd15ff0394827f073daf204f85f771a6b420edad2dd9c89a7039474dea075a35aa6ed6826b4533d5")
    , ("faio31221", "2f9c875f047a98f3de27278970da12b8a019a3e24418f8267c70194eb970f22dcda13084a5404c1251fa556ddb97df3a8d897979368f607979712cb3a7d190bd")
    , ("中文密码", "d548a8afaa7053425d7d7dadb406b54923a589d9af0d7124c7024f16a493dbbb70f2353c4660626f154eff3e3e1a1f274491474551ce4812bae5956b2691b36c")
    ]
{- Password generation steps:
  1. select a password's plain text. Example: abcdef
  2. prepend ****************************************************************
    to the password.
  3. Use an online SHA-3 calculator to hash the salted password
    Recommended calculator: https://emn178.github.io/online-tools/sha3_512.html
    (Example: ****************************************************************abcdef)
  4. Copy the hash result back (in hex)
    (Example: f2bec0968ad2b03069052e2343ffac08c519082221e46fa5fd15ff0394827f073daf204f85f771a6b420edad2dd9c89a7039474dea075a35aa6ed6826b4533d5)

  The salt is 64 asterisks (which has an ASCII code of 42), as defined below
-}

sampleFiles :: [(Common.File, ByteString)]
sampleFiles =
    [ (Common.File "16337060-zelinf.txt" "zelinf" emptyHash (UTCTime (fromGregorian 2019 5 6) 0), "Homework of zelinf")
    , (Common.File "16337063-lagrand.txt" "lagrand" emptyHash (UTCTime (fromGregorian 2019 5 5) (22 * 3600)), "Homework of lagrand")
    , (Common.File "hw3.txt" "lagrand" emptyHash (UTCTime (fromGregorian 2019 5 6) 0), "Sample homework 3\nlalala")
    , (Common.File "hw4.txt" "同学C" emptyHash (UTCTime (fromGregorian 2019 5 6) 0), "Sample homework 4\nlalala")
    , (Common.File "hw5.txt" "同学C" emptyHash (UTCTime (fromGregorian 2019 5 6) 0), "Sample homework 5\nhahaha")
    ]
  where
    emptyHash = Common.HashValue Common.HashTypeMD5 ""

salt :: ByteString
salt = BS.replicate 64 42

bsFromHex :: Text -> ByteString
bsFromHex hexStr =
    BS.pack $ fmap (fst . head . readHex . Text.unpack) hexStrs
  where
    hexStrs = Text.chunksOf 2 hexStr
