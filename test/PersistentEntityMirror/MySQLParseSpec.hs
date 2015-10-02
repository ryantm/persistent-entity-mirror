{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLParseSpec where




import Test.Hspec


import Database.PersistentEntityMirror.MySQLParse

spec :: Spec
spec = do
  describe "parseType" (do
    it "should parse out the bytes" (do
      parseType "char(1)" `shouldBe` ("char",1)))
