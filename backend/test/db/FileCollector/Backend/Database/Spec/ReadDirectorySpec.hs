{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Database.Spec.ReadDirectorySpec
  ( spec
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Time (UTCTime (..), fromGregorian)
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Test.Hspec

import           FileCollector.Backend.Database.Impl.ReadDirectory
import           FileCollector.Backend.Database.Spec.Util (withSqliteInMemory)
import           FileCollector.Backend.Database.Types.CanUploadTo
import           FileCollector.Backend.Database.Types.Directory
import           FileCollector.Backend.Database.Types.Role
import           FileCollector.Backend.Database.Types.UploadRule
import           FileCollector.Backend.Database.Types.User
import           FileCollector.Common.Base.Convertible
import qualified FileCollector.Common.Types.Directory as Common
import qualified FileCollector.Common.Types.User as Common

spec :: Spec
spec = parallel $ do
    describe "getAllDirectories" $
      it "gives all directories in a database" $ example $ withSqliteInMemory LevelWarn $ do
        expectedDirs <- insertSimpleData

        actualDirs <- getAllDirectories
        liftIO $ shouldMatchList actualDirs expectedDirs

    describe "getDirectory" $ do
      it "gives a specific directory" $ example $ withSqliteInMemory LevelWarn $ do
        _ <- insertSimpleData
        dirMaybe <- getDirectory "isumi" "目录-2"
        let actualTimeMaybe = dirMaybe >>= directoryExpirationTime
        liftIO $
          actualTimeMaybe `shouldBe` Just (UTCTime (fromGregorian 2019 5 7) 0)
      it "gives Nothing when directory is absent" $ example $ withSqliteInMemory LevelWarn $ do
        _ <- insertSimpleData
        dirMaybe <- getDirectory "isumi" "dir2"
        liftIO $ dirMaybe `shouldBe` Nothing

    describe "getVisibleDirectories" $
      it "gives a uploader's visible directories" $ example $ withSqliteInMemory LevelWarn $ do
        zelinfId <- insert $ User "zelinf" "123456" (convert Common.RoleUploader)
        isumiId <- insert $ User "isumi" "234567" (convert Common.RoleCollector)
        let dir1 = Directory "homework-1" isumiId Nothing [convert (Common.RuleMaxFiles 2)]
        let dir2 = Directory "other-lesson" isumiId Nothing []
        dir1Id <- insert dir1
        insert_ dir2
        insert_ $ CanUploadTo zelinfId dir1Id

        dirs <- getVisibleDirectories "zelinf"
        liftIO $ dirs `shouldBe` [dir1]

    describe "getOwnDirectories" $
      it "gives directories owned by a collector" $ example $ withSqliteInMemory LevelWarn $ do
        dirs <- insertSimpleData
        
        actualDirs <- getOwnDirectories "isumi"
        liftIO $ actualDirs `shouldMatchList` dirs

insertSimpleData :: MonadIO m
                 => ReaderT SqlBackend m [Directory]
insertSimpleData = do
    isumiId <- insert $ User "isumi" "123456" (convert Common.RoleAdmin)
    let expectedDirs =
          [ Directory "dir1" isumiId Nothing []
          , Directory "目录-2" isumiId (Just (UTCTime (fromGregorian 2019 5 7) 0)) []
          ]
    traverse_ insert_ expectedDirs
    pure expectedDirs
