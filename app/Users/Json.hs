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

instance ToJSON RegError where
  toEncoding = genericToEncoding defaultOptions

data RegFields = RegFields
  { _username :: Field [UserNameValidationError] UserName,
    _email :: Field [EmailValidationError] Email
  }

regFields_ =
  RegFields
    { _username = Field "username" FNull makeUsername,
      _email = Field "email" FRequired makeEmail
    }


regFields =
  [ ("username", FNull, SC makeUsername),
    ("email", FRequired, SC makeEmail)
  ]

instance JsonArgs RegArgsErrors RegArgs where
  parseCmdfromJson t =
    if is_errors -- hasErrors t regFields
      then
        Left $
          RegArgsErrors
            (gErrs_ t $ _username regFields_) -- (errs_ username_)
            (gErrs t $ _email regFields_) -- (errs email_)
      else
        Right $
          RegArgs
            (gRes_ t $ _username regFields_) -- (res_ username_)
            (gRes t $ _email regFields_) -- (res email_)
    where
      -- is_errors = e username_ || e_ email_
      is_errors = (isFerr t $ _username regFields_) || (isFerr t $ _email regFields_)

-- username_ = pf t "username" (smc $ _username regFields_)
-- email_ = pf t "email" (smc $ _email regFields_)

-- is_errors = e username_ || e_ email_
-- username_ = pf t "username" makeUsername
-- email_ = pf t "email" makeEmail

-- fields :: JsonArgs RegArgsErrors RegArgs => [(Key, FieldType, SC)]
