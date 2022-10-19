{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Commandy.Types
  ( ErrorOrResult (..),
    ResultOk_ (..),
    Email,
    makeEmail,
    EmailValidationError,
    UserName,
    makeUsername,
    UserNameValidationError,
    Password,
    PasswordValidationError,
    JWTToken,
  )
where

import Control.Lens hiding ((.=))
import Data.List (isInfixOf)
import Data.Validation
import GHC.Generics

-- ***** Types *****

newtype AtString = AtString String deriving (Show)

newtype PeriodString = PeriodString String deriving (Show)

newtype NonEmptyString = NonEmptyString String deriving (Show)

data ErrorOrResult e r = DError e | DResult r deriving (Generic, Show)

data ResultOk_ = ROK_ deriving (Generic, Show)

newtype Email = Email String deriving (Generic, Show)

data EmailValidationError
  = MustNotBeEmpty
  | MustContainAt
  | MustContainPeriod
  deriving (Generic, Show)

-- ***** Base smart constructors *****

-- String must contain an '@' character
atString :: String -> Validation [EmailValidationError] AtString
atString x =
  if "@" `isInfixOf` x
    then _Success # AtString x
    else _Failure # [MustContainAt]

-- String must contain an '.' character
periodString :: String -> Validation [EmailValidationError] PeriodString
periodString x =
  if "." `isInfixOf` x
    then _Success # PeriodString x
    else _Failure # [MustContainPeriod]

-- String must not be empty
nonEmptyString :: String -> Validation [EmailValidationError] NonEmptyString
nonEmptyString x =
  if x /= []
    then _Success # NonEmptyString x
    else _Failure # [MustNotBeEmpty]

-- ***** Combining smart constructors *****

makeEmail :: String -> Validation [EmailValidationError] Email
makeEmail x =
  Email x
    <$ nonEmptyString x
    <* atString x
    <* periodString x

newtype UserName = UserName String deriving (Generic, Show)

newtype NonSmallUsername = NonSmallUsername String deriving (Show)

newtype NonBigUsername = NonBigUsername String deriving (Show)

data UserNameValidationError
  = ToBigUserLength Int Int
  | ToSmallUserLength Int Int
  deriving (Generic, Show)

--  InvalidUserChars String String deriving Show

nonSmallUsername :: String -> Validation [UserNameValidationError] NonSmallUsername
nonSmallUsername x =
  if length x < 3
    then _Failure # [ToSmallUserLength (length x) 6]
    else _Success # NonSmallUsername x

nonBigUsername :: String -> Validation [UserNameValidationError] NonBigUsername
nonBigUsername x =
  if length x > 12
    then _Failure # [ToBigUserLength (length x) 12]
    else _Success # NonBigUsername x

makeUsername :: String -> Validation [UserNameValidationError] UserName
makeUsername x =
  UserName x
    <$ nonBigUsername x
    <* nonSmallUsername x

type Password = String

type JWTToken = String

data PasswordValidationError
  = ToBigPassLength Int Int
  | ToSmallPassLength Int Int
  | InvalidPassChars String String
  deriving (Generic, Show)