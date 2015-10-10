{-# LANGUAGE OverloadedStrings #-}

module Database.PersistentEntityMirror.MySQL where

import Database.PersistentEntityMirror.MySQLParse

import qualified Data.ByteString as BS
import           Data.ByteString.Builder
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
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

fieldName :: MySQLDescribe -> Text
fieldName (Just name,_,_,_,_,_) = name


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
    connectDatabase = T.unpack database }
  tables <- tablesOfDatabase conn
  keyValues <- mapM (\ table -> do
    description <- descriptionOfTable conn table
    return (table, description)) tables
  return (HM.fromList keyValues)


-- | Given a database connection, return a list of table names in that
-- database
tablesOfDatabase :: Connection -> -- ^ a mysql-simple connection
                    IO [Text] -- ^ a list of table names
tablesOfDatabase conn =
  (fmap . fmap) fromOnly (query_ conn "show tables")



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



  -- "GlobalVariable\n\
  -- \  variableName Text sqltype=varchar(64)\n\
  -- \  variableValue Text sqltype=varchar(1024) Maybe default=Nothing"

tableNameMapping :: Text -> Text
tableNameMapping "GLOBAL_VARIABLES" = "GlobalVariable sql=GLOBAL_VARIABLES"
tableNameMapping "STATISTICS" = "Statistics sql=STATISTICS"

fieldNameMapping :: Text -> Text
fieldNameMapping t =
  let first:rest = T.splitOn "_" t in
  T.concat ([T.toLower first] ++ map T.toTitle rest)

fieldTypeMapping :: Text ->  Text
fieldTypeMapping = fieldTypeNameBytesMapping . parseType

fieldTypeNameBytesMapping :: (Text, Int) -> Text
fieldTypeNameBytesMapping ("varchar", b) =
  "Text sqltype=varchar(" `T.append` T.pack (show b) `T.append` ")"
fieldTypeNameBytesMapping ("bigint", b) =
  "Int sqltype=bigint(" `T.append` T.pack (show b) `T.append` ")"
fieldTypeNameBytesMapping a = error (show a)

nullMapping :: Text -> Maybe Text -> [Text]
nullMapping "YES" Nothing = ["Maybe default=Nothing"]
nullMapping "NO" _ = []

fieldDefaultMapping :: Maybe Text -> [Text]
fieldDefaultMapping Nothing = []
fieldDefaultMapping (Just "") = []
fieldDefaultMapping (Just d) = ["default=" `T.append` d]


descriptionToEntityTemplate :: Text -> -- ^ table name
                               [MySQLDescribe] -> -- ^ description of table fields
                               Text
descriptionToEntityTemplate tablename rows =
  T.unlines ([ tableNameMapping tablename ] ++ map rowToEntityField rows)
  where
    rowToEntityField :: MySQLDescribe -> Text
    rowToEntityField
      (Just name, Just fieldType, Just null, key, fieldDefault, extra) =
      "  " `T.append` T.unwords ([
          fieldNameMapping name
        , fieldTypeMapping fieldType
        ] ++ fieldDefaultMapping fieldDefault ++ nullMapping null fieldDefault)
