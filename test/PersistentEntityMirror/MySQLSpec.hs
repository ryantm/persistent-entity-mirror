{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLSpec where

import Data.HashMap.Strict ((!))
import Data.Maybe
import Data.Text hiding (any)
import Database.MySQL.Simple


import Test.Hspec

import Database.PersistentEntityMirror.MySQL


-- describe GLOBAL_VARIABLES;
-- +----------------+---------------+------+-----+---------+-------+
-- | Field          | Type          | Null | Key | Default | Extra |
-- +----------------+---------------+------+-----+---------+-------+
-- | VARIABLE_NAME  | varchar(64)   | NO   |     |         |       |
-- | VARIABLE_VALUE | varchar(1024) | YES  |     | NULL    |       |
-- +----------------+---------------+------+-----+---------+-------+
-- 2 rows in set (0.00 sec)


globalVariablesDescription :: [MySQLDescribe]
globalVariablesDescription = [
  ( Just "VARIABLE_NAME"
  , Just "varchar(64)"
  , Just "NO"
  , Just ""
  , Just ""
  , Just ""),
  ( Just "VARIABLE_VALUE"
  , Just "varchar(1024)"
  , Just "YES"
  , Just ""
  , Nothing
  , Just "")]


globalVariableEntityTemplate :: Text
globalVariableEntityTemplate =
  "GlobalVariable sql=GLOBAL_VARIABLES\n\
  \  variableName Text sqltype=varchar(64)\n\
  \  variableValue Text sqltype=varchar(1024) Maybe default=Nothing\n"

{-
MariaDB [information_schema]> describe schemata;
+----------------------------+--------------+------+-----+---------+-------+
| Field                      | Type         | Null | Key | Default | Extra |
+----------------------------+--------------+------+-----+---------+-------+
| CATALOG_NAME               | varchar(512) | NO   |     |         |       |
| SCHEMA_NAME                | varchar(64)  | NO   |     |         |       |
| DEFAULT_CHARACTER_SET_NAME | varchar(32)  | NO   |     |         |       |
| DEFAULT_COLLATION_NAME     | varchar(32)  | NO   |     |         |       |
| SQL_PATH                   | varchar(512) | YES  |     | NULL    |       |
+----------------------------+--------------+------+-----+---------+-------+
5 rows in set (0.00 sec)
-}


schemataDescription :: [MySQLDescribe]
schemataDescription = [
  ( Just "CATALOG_NAME"
  , Just "varchar(512)"
  , Just "NO"
  , Just ""
  , Just ""
  , Just ""),
  ( Just "SCHEMA_NAME"
  , Just "varchar(64)"
  , Just "NO"
  , Just ""
  , Just ""
  , Just ""),
  ( Just "DEFAULT_CHARACTER_SET_NAME"
  , Just "varchar(32)"
  , Just "NO"
  , Just ""
  , Just ""
  , Just ""),
  ( Just "DEFAULT_COLLATION_NAME"
  , Just "varchar(32)"
  , Just "NO"
  , Just ""
  , Just ""
  , Just ""),
  ( Just "SQL_PATH"
  , Just "varchar(512)"
  , Just "YES"
  , Just ""
  , Nothing
  , Just "")]

-- describe STATISTICS;
-- +---------------+---------------+------+-----+---------+-------+
-- | Field         | Type          | Null | Key | Default | Extra |
-- +---------------+---------------+------+-----+---------+-------+
-- | TABLE_CATALOG | varchar(512)  | NO   |     |         |       |
-- | TABLE_SCHEMA  | varchar(64)   | NO   |     |         |       |
-- | TABLE_NAME    | varchar(64)   | NO   |     |         |       |
-- | NON_UNIQUE    | bigint(1)     | NO   |     | 0       |       |
-- | INDEX_SCHEMA  | varchar(64)   | NO   |     |         |       |
-- | INDEX_NAME    | varchar(64)   | NO   |     |         |       |
-- | SEQ_IN_INDEX  | bigint(2)     | NO   |     | 0       |       |
-- | COLUMN_NAME   | varchar(64)   | NO   |     |         |       |
-- | COLLATION     | varchar(1)    | YES  |     | NULL    |       |
-- | CARDINALITY   | bigint(21)    | YES  |     | NULL    |       |
-- | SUB_PART      | bigint(3)     | YES  |     | NULL    |       |
-- | PACKED        | varchar(10)   | YES  |     | NULL    |       |
-- | NULLABLE      | varchar(3)    | NO   |     |         |       |
-- | INDEX_TYPE    | varchar(16)   | NO   |     |         |       |
-- | COMMENT       | varchar(16)   | YES  |     | NULL    |       |
-- | INDEX_COMMENT | varchar(1024) | NO   |     |         |       |
-- +---------------+---------------+------+-----+---------+-------+

statisticsEntityTemplate :: Text
statisticsEntityTemplate =
  "Statistics sql=STATISTICS\n\
  \  tableCatalog Text sqltype=varchar(512)\n\
  \  tableSchema Text sqltype=varchar(64)\n\
  \  tableName Text sqltype=varchar(64)\n\
  \  nonUnique Int sqltype=bigint(1) default=0\n\
  \  indexSchema Text sqltype=varchar(64)\n\
  \  indexName Text sqltype=varchar(64)\n\
  \  seqInIndex Int sqltype=bigint(2) default=0\n\
  \  columnName Text sqltype=varchar(64)\n\
  \  collation Text sqltype=varchar(1) Maybe default=Nothing\n\
  \  cardinality Int sqltype=bigint(21) Maybe default=Nothing\n\
  \  subPart Int sqltype=bigint(3) Maybe default=Nothing\n\
  \  packed Text sqltype=varchar(10) Maybe default=Nothing\n\
  \  nullable Text sqltype=varchar(3)\n\
  \  indexType Text sqltype=varchar(16)\n\
  \  comment Text sqltype=varchar(16) Maybe default=Nothing\n\
  \  indexComment Text sqltype=varchar(1024)\n"


spec :: Spec
spec = do
  describe "mysql service dependency" ( do
    it "should be running" ( do
      conn <- connect defaultConnectInfo
      x:_ <- query_ conn "select 2 + 2" :: IO [Only Int]
      x `shouldBe` Only 4)
    it "should have a GLOBAL_VARIABLES table in the information_schema db" ( do
      conn <- connect defaultConnectInfo {
        connectDatabase = "information_schema" }
      result <- (query_ conn "describe GLOBAL_VARIABLES") :: IO [MySQLDescribe]
      result `shouldBe` globalVariablesDescription))
  describe "descriptionOfTable" ( do
    it "should return a list of descriptions" ( do
      conn <- connect defaultConnectInfo {
        connectDatabase = "information_schema" }
      result <- descriptionOfTable conn "GLOBAL_VARIABLES"
      result `shouldBe` globalVariablesDescription))
  describe "descriptionOfDatabase" (do
    it "should return a hashmap with keys as table names and values as lists of descriptions" (do
      result <- descriptionOfDatabase "information_schema"
      result ! "GLOBAL_VARIABLES" `shouldBe` globalVariablesDescription
      result ! "SCHEMATA" `shouldBe` schemataDescription))
  describe "tablesOfDatabase" (do
    specify "the information_schema should include GLOBAL_VARIABLES" (do
      conn <- connect defaultConnectInfo {
        connectDatabase = "information_schema" }
      result <- tablesOfDatabase conn
      result `shouldSatisfy` any (== "GLOBAL_VARIABLES")))
  describe "descriptionToEntityTemplate" (do
    it "should create an entity template for GLOBAL_VARIABLES" (do
      descriptionToEntityTemplate "GLOBAL_VARIABLES" globalVariablesDescription
        `shouldBe` globalVariableEntityTemplate)
    it "should create an entity template for STATISTICS" (do
      conn <- connect defaultConnectInfo {
        connectDatabase = "information_schema" }
      desc <- descriptionOfTable conn "STATISTICS"
      descriptionToEntityTemplate "STATISTICS" desc
        `shouldBe` statisticsEntityTemplate))
