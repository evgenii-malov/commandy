{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.Types where

import Commandy
import Control.Exception
import Data.Commandy.Types
import Data.Int (Int64)
import Data.Maybe
import Database.MySQL.Base (MySQLError (errNumber))
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import GHC.Generics

-- best loacted not here? req for core.. - this errors out of domain (more like parsing errs)

data RegArgsErrors = RegArgsErrors
  { username_error :: [UserNameValidationError],
    email_error :: FieldRequired [EmailValidationError]
  }
  deriving (Generic, Show)

data RegArgs = RegArgs
  { username :: Maybe UserName,
    email :: Email
  }
  deriving (Show)

data RegError
  = UserExists UserName
  | EmailUsed Email
  | OtherRegError String
  deriving (Generic, Show)

reg :: RegArgs -> IO (ErrorOrResult RegError ResultOk_)
reg (args@RegArgs {username = username, email = email}) = do
  -- error "catch me!"
  print args
  let uname = fromMaybe "NULL" (show <$> username)
  let qargs = [show email, "16", uname]
  conn <- connect defaultConnectInfo {connectPassword = "root", connectDatabase = "test"}

  do
    r <- (try $ execute conn ("insert into user (email,age,name) values (?,?,?)") qargs) :: IO (Either MySQLError Int64)
    case r of
      Left e -> if errNumber e == 1062 then return . DError $ EmailUsed email else error "something WRONG!"
      Right _ -> do uid <- insertID conn; print $ "uid" ++ show uid; return $ DResult ROK_

data LoginArgs = LoginArgs
  { email :: Email,
    pwd :: Password
  }
  deriving (Show)

data LoginError
  = UserBanned Email
  | OtherLoginError String
  deriving (Generic, Show)

login :: LoginArgs -> IO (ErrorOrResult RegError ResultOk_)
login args = do
  putStrLn "implement login please!"
  return $ DResult ROK_