{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLSpec where

import Test.Hspec

import Database.MySQL.Simple


-- describe GLOBAL_VARIABLES;
-- +----------------+---------------+------+-----+---------+-------+
-- | Field          | Type          | Null | Key | Default | Extra |
-- +----------------+---------------+------+-----+---------+-------+
-- | VARIABLE_NAME  | varchar(64)   | NO   |     |         |       |
-- | VARIABLE_VALUE | varchar(1024) | YES  |     | NULL    |       |
-- +----------------+---------------+------+-----+---------+-------+
-- 2 rows in set (0.00 sec)

spec :: Spec
spec =
  describe "mysql service dependency" (
    it "should be running" (
        do
          conn <- connect defaultConnectInfo
          result <- query_ conn "select 2 + 2"
          case result of
            [] -> do
              putStrLn "database returned no results"
              1 `shouldBe` 2 -- always fail
            x:_ -> (x :: Only Int) `shouldBe` Only 4))
