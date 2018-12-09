{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Isumi.FileCollector.Server.Persist.Entity.FileLocation
  ( FileLocation(..)
  ) where

import Database.Persist.TH

data FileLocation = FileLocationLocalDisk FilePath
                    deriving (Show, Read, Eq)

derivePersistField "FileLocation"
