-- Some comments from this file are copied from the MySQL Reference Manual section on Data Types

{-# LANGUAGE OverloadedStrings #-}

module Database.PersistentEntityMirror.MySQLParse where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Text


-- TODO: handle ENUM and SET types

data MySQLTypeDescription = MySQLTypeDescription {
  --  M indicates the maximum display width for integer types. For floating-point and fixed-point types, M is the total number of digits that can be stored (the precision). For string types, M is the maximum length. The maximum permissible value of M depends on the data type.
    _m :: Int
  --  D applies to floating-point and fixed-point types and indicates the number of digits following the decimal point (the scale). The maximum possible value is 30, but should be no greater than M−2.
  , _d :: Int
  , _type :: Text
  , _signed :: Bool
  , _zerofill :: Bool
  , _characterSet :: Text
  , _collate :: Text
  }

parseType :: Text -> (Text, Int)
parseType t =
  let parsed = parse mysqlTypeParser "(source)" (T.unpack t) in
  case parsed of
    Right mType -> (_type mType, _m mType)
    Left e -> error (show e)


mysqlTypeParser :: Parsec String () MySQLTypeDescription
mysqlTypeParser = do
  name <- many1 letter
  (m, maybeD) <- option (0, Nothing) mysqlTypeBytes
  eof
  return (MySQLTypeDescription {_type = T.pack name, _m = m })

mysqlTypeBytes :: Parsec String () (Int, Maybe Int)
mysqlTypeBytes = do
  char '('
  m <- many1 digit
  d <- option Nothing (do
                      char ','
                      d' <- many1 digit
                      return (Just d'))
  char ')'
  return (read m, fmap read d)
