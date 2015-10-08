{-# LANGUAGE OverloadedStrings #-}

module Database.PersistentEntityMirror.MySQLParse where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Text

data MySQLType = MySQLType String String

parseType :: Text -> (Text, Text)
parseType t =
  let Right (MySQLType name bytes) = parse mysqlTypeParser "(source)" (T.unpack t) in
  (T.pack name, T.pack bytes)


mysqlTypeParser :: Parsec String () MySQLType
mysqlTypeParser = do
  name <- many1 letter
  bytes <- option "" mysqlTypeBytes
  eof
  return (MySQLType name bytes)

mysqlTypeBytes :: Parsec String () String
mysqlTypeBytes = do
  char '('
  bytes <- many1 digit
  char ')'
  return bytes
