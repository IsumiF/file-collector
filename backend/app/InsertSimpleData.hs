module Main
  ( main
  ) where

import Database.Persist.Sqlite (runSqlite)
import System.Environment (getArgs)
import qualified Data.Text as T

import FileCollector.Backend.TestData.Simple (populateTestData)

main :: IO ()
main = do
    [dbPath] <- getArgs
    runSqlite (T.pack dbPath) populateTestData
