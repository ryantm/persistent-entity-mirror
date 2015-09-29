{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PersistentEntityMirror.MySQLManualSpec where

import Database.Persist.MySQL
import Database.Persist.Quasi
import Database.Persist.TH
import Language.Haskell.TH.Quote
import Data.Text

import Test.Hspec


description = "GlobalVariable sql=GLOBAL_VARIABLES\n  variableName Text sqltype=varchar(64)\n  variableValue Text sqltype=varchar(1024) Maybe default=Nothing\n"


mkPersist sqlSettings (parse lowerCaseSettings "GlobalVariable sql=GLOBAL_VARIABLES\n  variableName Text sqltype=varchar(64)\n  variableValue Text sqltype=varchar(1024) Maybe default=Nothing\n")

spec = undefined
