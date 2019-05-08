{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Database.Spec.TestData
  ( populateTestData
  , samplePasswords
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.List ((!!))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime (..), fromGregorian)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Numeric (readHex)

import           FileCollector.Backend.Database.Types.CanUploadTo
import           FileCollector.Backend.Database.Types.Directory
import           FileCollector.Backend.Database.Types.Role
import           FileCollector.Backend.Database.Types.UploadRule
import           FileCollector.Backend.Database.Types.User
import           FileCollector.Common.Base.Convertible
import qualified FileCollector.Common.Types.Directory as Common
import qualified FileCollector.Common.Types.User as Common

{-| Insert test data into database
-}
populateTestData :: (MonadIO m, MonadReader SqlBackend m)
                 => m ()
populateTestData = liftPersist $ do
    insert_ $ User "Isumi Fly" (snd $ samplePasswords !! 1) (convert Common.RoleAdmin)
    ta1 <- insert $ User "TA-01" (snd $ samplePasswords !! 0) (convert Common.RoleCollector)
    ta2 <- insert $ User "助教-02" (snd $ samplePasswords !! 2) (convert Common.RoleCollector)
    stu1 <- insert $ User "zelinf" (snd $ samplePasswords !! 0) (convert Common.RoleUploader)
    stu2 <- insert $ User "lagrand" (snd $ samplePasswords !! 0) (convert Common.RoleUploader)
    stu3 <- insert $ User "同学C" (snd $ samplePasswords !! 1) (convert Common.RoleUploader)

    let lesson1Rules :: [UploadRule] = fmap convert
          [ Common.RuleMaxFiles 1
          , Common.RuleMaxFileSize (64 * 1024 * 1024)
          , Common.RuleFileNameFormat "[[:digit:]]{8}-.+\\.[[:alnum:]\\.]+"
          ]

    dir1 <- insert $ Directory "课程1-作业1" ta1
      (Just $ UTCTime (fromGregorian 2019 5 8) 0)
      lesson1Rules
    dir2 <- insert $ Directory "课程1-作业2" ta1
      (Just $ UTCTime (fromGregorian 2019 6 8) 0)
      lesson1Rules
    dir3 <- insert $ Directory "课程2-大作业" ta2
      Nothing
      ( fmap convert
        [ Common.RuleMaxFiles 2
        , Common.RuleMaxFileSize (30 * 1024 * 1024)
        ]
      )

    insert_ $ CanUploadTo stu1 dir1
    insert_ $ CanUploadTo stu1 dir2
    insert_ $ CanUploadTo stu2 dir2
    insert_ $ CanUploadTo stu2 dir3
    insert_ $ CanUploadTo stu3 dir1
    insert_ $ CanUploadTo stu3 dir3


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

salt :: ByteString
salt = ByteString.replicate 64 42

bsFromHex :: Text -> ByteString
bsFromHex hexStr =
    ByteString.pack $ fmap (fst . head . readHex . Text.unpack) hexStrs
  where
    hexStrs = Text.chunksOf 2 hexStr
