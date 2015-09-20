{-# LANGUAGE OverloadedStrings #-}

module Database.PersistentEntityMirror.MySQL where

import qualified Data.ByteString as BS
import           Data.ByteString.Builder
import           Data.HashMap.Strict as HM
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
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


type DatabaseDescription = HM.HashMap Text [MySQLDescribe]


-- | Given a database name, returns a DatabaseDescription in IO
--
-- Warning: The caller is responsible for making sure the database
-- name is properly formatted, that is, this function does not protect
-- against SQL injection.
descriptionOfDatabase :: Text -> -- ^ a database name
                         IO DatabaseDescription
descriptionOfDatabase database = do
  conn <- connect defaultConnectInfo {
    connectDatabase = unpack database }
  tables <- tablesOfDatabase conn
  keyValues <- mapM (\ table -> do
    description <- descriptionOfTable conn table
    return (table, description)) tables
  return (HM.fromList keyValues)


-- | Given a database connection, return a list of table names in that
-- database
tablesOfDatabase :: Connection -> -- ^ a mysql-simple connection
                    IO [Text] -- ^ a list of table names
tablesOfDatabase conn = do
  result <- query_ conn "show tables"
  return (fmap fromOnly result)


-- | Given a table name and a database connection it returns a list of
-- description of the rows of the table in IO
--
-- Warning: The caller is responsible for making sure the table name
-- is properly formatted, that is, this function does not protect
-- against SQL injection.
descriptionOfTable :: Connection ->  -- ^ a mysql-simple connection
                      Text ->  -- ^ table name
                      IO [MySQLDescribe]
descriptionOfTable conn table = do
  query conn "describe ?" (Only (Plain (byteString (encodeUtf8 table))))
