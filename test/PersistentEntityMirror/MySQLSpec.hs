{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLSpec where

import Control.Monad (forM_)
import Data.Maybe
import Data.Text as Text
import Database.MySQL.Simple
import Database.MySQL.Simple.Result (Result(..))


import Test.Hspec



-- describe GLOBAL_VARIABLES;
-- +----------------+---------------+------+-----+---------+-------+
-- | Field          | Type          | Null | Key | Default | Extra |
-- +----------------+---------------+------+-----+---------+-------+
-- | VARIABLE_NAME  | varchar(64)   | NO   |     |         |       |
-- | VARIABLE_VALUE | varchar(1024) | YES  |     | NULL    |       |
-- +----------------+---------------+------+-----+---------+-------+
-- 2 rows in set (0.00 sec)

alwaysFail = 1 `shouldBe` 2

spec :: Spec
spec =
  describe "mysql service dependency" ( do
    it "should be running" (
        do
          conn <- connect defaultConnectInfo
          result <- query_ conn "select 2 + 2"
          case result of
            [] -> do
              putStrLn "database returned no results"
              alwaysFail
            x:_ -> (x :: Only Int) `shouldBe` Only 4)
    it "should have a GLOBAL_VARIABLES table in the information_schema db" (
        do
          conn <- connect defaultConnectInfo {
            connectDatabase = "information_schema" }
          (a,b,c,d,e,f):_ <- query_ conn "describe GLOBAL_VARIABLES"
          (a :: Maybe Text) `shouldBe` Just "VARIABLE_NAME"
          (b :: Maybe Text) `shouldBe` Just "varchar(64)"
          (c :: Maybe Text) `shouldBe` Just "NO"
          (d :: Maybe Text) `shouldBe` Just ""
          (e :: Maybe Text) `shouldBe` Just ""
          (f :: Maybe Text) `shouldBe` Just ""))
