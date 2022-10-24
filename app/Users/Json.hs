{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.Json where

import Commandy
import Data.Aeson
import qualified Data.ByteString as LB
import Data.Commandy.Json
import Data.Commandy.Types
import Users.Args
import Users.Types

instance ToJSON RegArgsErrors where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON LoginArgsErrors where
  toEncoding = genericToEncoding defaultOptions  

instance ToJSON RegError where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON NoErrors where
  toEncoding = genericToEncoding defaultOptions  

instance ToJSON UsersListArgs where
  toEncoding = genericToEncoding defaultOptions    
