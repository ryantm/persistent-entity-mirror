{-# LANGUAGE OverloadedStrings #-}

module PersistentEntityMirror.MySQLManualSpec where

import Database.Persist.MySQL
import Database.Persist.TH
import Language.Haskell.TH.Quote

import Test.Hspec


description = "GlobalVariable sql=GLOBAL_VARIABLES\n  variableName Text sqltype=varchar(64)\n  variableValue Text sqltype=varchar(1024) Maybe default=Nothing\n"

mkPersist sqlSettings (quoteExp persistLowerCase) description
