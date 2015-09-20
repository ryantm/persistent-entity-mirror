{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLSpec where

import Data.HashMap.Strict ((!))
import Data.Maybe
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
      result ! "GLOBAL_VARIABLES" `shouldBe` globalVariablesDescription))
  describe "tablesOfDatabase" (do
    specify "the information_schema should include GLOBAL_VARIABLES" (do
      conn <- connect defaultConnectInfo {
        connectDatabase = "information_schema" }
      result <- tablesOfDatabase conn
      result `shouldSatisfy` any (== "GLOBAL_VARIABLES")))
