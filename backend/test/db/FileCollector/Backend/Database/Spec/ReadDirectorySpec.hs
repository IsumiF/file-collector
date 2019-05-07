{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Database.Spec.ReadDirectorySpec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Time (UTCTime (..), fromGregorian)
import Database.Persist
import Test.Hspec

import           FileCollector.Backend.Database.Impl.ReadDirectory
    (getAllDirectories)
import           FileCollector.Backend.Database.Spec.Util (withSqliteInMemory)
import           FileCollector.Backend.Database.Types.Directory
import           FileCollector.Backend.Database.Types.Role
import           FileCollector.Backend.Database.Types.User
import           FileCollector.Common.Base.Convertible
import qualified FileCollector.Common.Types.User as Common

spec :: Spec
spec =
    describe "getAllDirectories" $
      it "gives all directories in a database" $ example $ withSqliteInMemory $ do
        isumiId <- insert $ User "isumi" "123456" (convert Common.RoleAdmin)
        let expectedDirs =
              [ Directory "dir1" isumiId Nothing []
              , Directory "目录-2" isumiId (Just (UTCTime (fromGregorian 2019 5 7) 0)) []
              ]
        traverse_ insert_ expectedDirs

        actualDirs <- getAllDirectories
        liftIO $ shouldMatchList actualDirs expectedDirs
