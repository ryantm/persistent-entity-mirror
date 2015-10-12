{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLParseSpec where

import Control.Exception (evaluate)


import Test.Hspec


import Database.PersistentEntityMirror.MySQLParse

spec :: Spec
spec = do
  describe "parseType" (do
    it "should parse out the bytes" (do
      parseType "char(1)" `shouldBe` ("char",1))
    specify "bytes are optional" (do
      parseType "char" `shouldBe` ("char",0))
    specify "should work with d and m" (do
      parseType "DECIMAL(5,2)" `shouldBe` ("DECIMAL",5))
    it "should work with unsigned and zerofill" (do
      parseType "FLOAT(5,2) UNSIGNED ZEROFILL" `shouldBe` ("FLOAT",5))
    it "should not parse unmatched parens" (do
      evaluate (parseType "bigint(5") `shouldThrow` anyException))
