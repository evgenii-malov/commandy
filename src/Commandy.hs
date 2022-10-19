{-# LANGUAGE OverloadedStrings #-}
module Commandy where

import Data.Maybe
import Data.Validation

-- Field required err or actual fields errors
data FieldRequired a = FieldRequired | FieldErrors a deriving (Show)

data FieldType = FNull | FRequired deriving (Show,Eq)

data Field e r = Field {
  name :: String,
  ftype :: FieldType,
  smc :: String -> Validation e r
  --default ??
}

-- check that required field has error or not
e :: Maybe (Either a b) -> Bool
e Nothing = True
e (Just (Left _)) = True
e _ = False

-- check that nullable field has error or not
e_ :: Maybe (Either a b) -> Bool
e_ Nothing = False
e_ (Just (Left _)) = True
e_ _ = False

-- extract error of required field
errs :: Monoid a => Maybe (Either a b) -> FieldRequired a
errs (Just (Left a)) = FieldErrors a
errs Nothing = FieldRequired
errs _ = FieldErrors mempty

-- errs _ = error "use only to extract errors (not succes results)"

-- extract error of an null field
errs_ :: Monoid a => Maybe (Either a b) -> a
errs_ (Just (Left a)) = a
errs_ _ = mempty

-- errs_ _ = error "use only to extract errors (not succes results)"

-- extract result of req field
res :: Maybe (Either a b) -> b
res (Just (Right b)) = b
res _ = error "use only to extract succes results"

-- extract result of null field
res_ :: Maybe (Either a b) -> Maybe b
res_ (Just (Right b)) = Just b
res_ Nothing = Nothing
res_ _ = error "use only to extract succes results"


