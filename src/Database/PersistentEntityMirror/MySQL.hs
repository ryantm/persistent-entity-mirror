{-# LANGUAGE OverloadedStrings #-}

module Database.PersistentEntityMirror.MySQL where

import qualified Data.ByteString as BS
import           Data.ByteString.Builder
import           Data.Text
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Param

type MySQLDescribe = (
    Maybe Text
  , Maybe Text
  , Maybe Text
  , Maybe Text
  , Maybe Text
  , Maybe Text
  )

-- | Given a table name and a database name it returns a list of
-- description of the rows of the table.
--
-- Warning: The caller is responsbile for making the table and
-- database be properly formatted, that is, this function does not
-- protect against SQL injection.
descriptionOf :: String ->  -- ^ table name
                 String ->  -- ^ database name
                 IO [MySQLDescribe]
descriptionOf table database = do
  conn <- connect defaultConnectInfo {
    connectDatabase = table }
  query conn "describe ?" (Only (Plain (stringUtf8 database)))
