{-# LANGUAGE OverloadedStrings #-}

module Isumi.FileCollector.IntTest.DbGen.Simple
  ( genDbSimple
  ) where

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Time
    (LocalTime (LocalTime), TimeOfDay (..), UTCTime, fromGregorian,
    localTimeToUTC, utc)
import Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Isumi.FileCollector.Server.Persist.User (mkUser)
import Isumi.FileCollector.Server.Persist.Entity

genDbSimple :: FilePath -> IO ()
genDbSimple file = do
    runSqlite (fromString file) $ do
      runMigration migrateAll

      users <- liftIO $ sequenceA
        [ mkUser "admin" RoleAdmin (Just "Administrator") "admin"
        , mkUser "isumi" RoleCollector (Just "Isumi Fly") "pwd123"
        , mkUser "alice" RoleUploader Nothing "123456"
        ]
      [ _, isumiId, aliceId ] <- traverse insert users

      let dirs =
            [ Directory isumiId "stat04" Nothing
            ]
      [ stat04Id ] <- traverse insert dirs

      let files =
            [ File "" "a.zip" stat04Id aliceId
                (utcTime 2018 12 2 13 3 42)
                (fileLocal "data/a.zip")
            ]
      traverse_ insert files

utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcTime y mon d h m s = localTimeToUTC utc (LocalTime day timeInDay)
  where
    day = fromGregorian y mon d
    timeInDay = TimeOfDay h m (fromIntegral s)

fileLocal :: FilePath -> FileLocation
fileLocal = FileLocationLocalDisk

