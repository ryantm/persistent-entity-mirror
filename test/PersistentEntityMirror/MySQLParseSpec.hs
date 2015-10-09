{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLParseSpec where

import Control.Exception (evaluate)


import Test.Hspec


import Database.PersistentEntityMirror.MySQLParse

spec :: Spec
spec = do
  describe "parseType" (do
    it "should parse out the bytes" (do
      parseType "char(1)" `shouldBe` ("char","1"))
    specify "bytes are optional" (do
      parseType "char" `shouldBe` ("char",""))
    it "should not parse unmatched parens" (do
      evaluate (parseType "bigint(5") `shouldThrow` anyException))
