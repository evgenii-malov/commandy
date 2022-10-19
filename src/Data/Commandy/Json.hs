{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Commandy.Json where

import Commandy
import Data.Aeson
import qualified Data.Aeson.Key as K
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import Data.Commandy.Types
import Data.List
import Data.Maybe
import Data.Validation

exF :: FromJSON b => LB.ByteString -> Key -> Maybe b
exF t n = do
  result <- decode t -- "{\"name\":\"Dave\",\"age\":2}"
  flip parseMaybe result $ \obj -> obj .: n

-- pReq :: LB.ByteString -> Key -> (String -> Validation a b) -> Either a b
-- pReq d k f = toEither $ f $ fromJust $ exF d k

-- pNull :: LB.ByteString -> Key -> (String -> Validation a b) -> Maybe (Either a b)
-- pNull d k f = toEither.f <$> exF d k

-- extract field from json and validate it with smartconstructor
pf :: LB.ByteString -> Key -> (String -> Validation a b) -> Maybe (Either a b)
pf d k f = toEither . f <$> exF d k

-- LB.ByteString -> Key -> (String -> Validation a b) -> Maybe (Either a b)

-- username_ = pf t "username" (smc $ _username regFields_)
gErrs :: Monoid e => LB.ByteString -> Field e r -> FieldRequired e
gErrs td fl = errs pf_
  where
    pf_ = pf td n (smc fl)
    n = K.fromString . name $ fl

gErrs_ :: Monoid e => LB.ByteString -> Field e r -> e
gErrs_ td fl = errs_ pf_
  where
    pf_ = pf td n (smc fl)
    n = K.fromString . name $ fl

gRes :: LB.ByteString -> Field e r -> r
gRes td fl = res pf_
  where
    pf_ = pf td n (smc fl)
    n = K.fromString . name $ fl

gRes_ :: LB.ByteString -> Field e r -> Maybe r
gRes_ td fl = res_ pf_
  where
    pf_ = pf td n (smc fl)
    n = K.fromString . name $ fl

isFerr :: LB.ByteString -> Field e r -> Bool
isFerr td fl = if t == FNull then e_ pf_ else e pf_
  where
    n = K.fromString . name $ fl
    sc = smc fl
    t = ftype fl
    pf_ = pf td n sc

-- res :: T -> Field e r -> r

-- class SC f where
--   fapp :: String -> f -> Validation a b

-- smart constructor
data SC = forall a b. SC (String -> Validation a b)

data SMC a b = SMC (String -> Validation a b)

-- instance SC FN where
--   fapp s (FN f) = f s

-- t :: [(Key, FieldType, SC)] -> [(Key, FieldType, (String -> Validation a b))]
-- t ks = [(k,ft,f) | (k,ft,SC f) <- ks ]

-- gErr :: LB.ByteString -> [(Key, FieldType, (String -> Validation a b))] -> Key -> a
-- gErr = undefined

-- gErr :: (Monoid c) => LB.ByteString -> [(Key, FieldType, SC)] -> Key -> c
-- gErr t fs sk =  if ft == FRequired then errs $ pf t sk sc
--                 else errs_ $ pf t sk sc
--                 where
--                 --(ft,sc) = head [(ft,sc) | (k,ft,SC sc) <- fs , sk == k]
--                 (_,ft,SC sc) = fromJust $ find (\(k,_,_) -> k == sk) fs

-- gRes :: LB.ByteString -> [(Key, FieldType, (String -> Validation a b))] -> Key -> b
-- gRes = undefined

hasErrors :: LB.ByteString -> [(Key, FieldType, SC)] -> Bool
hasErrors t fields = any id [if ft == FNull then e_ $ pf t fn sc else e $ pf t fn sc | (fn, ft, (SC sc)) <- fields]

class JsonArgs e r | r -> e where
  parseCmdfromJson :: LB.ByteString -> Either e r

-- fields :: [(Key,FieldType,SC)] -- AllowAmbiguousTypes ?

-- collect_errors :: a -> Maybe T.Text -- json errors
-- is_success :: a -> Bool

router_ :: (ToJSON de, ToJSON dr, ToJSON ae, JsonArgs ae ar) => (Either ae ar) -> (ar -> IO (ErrorOrResult de dr)) -> IO LB.ByteString
router_ pd df = case pd of
  (Left es) -> do
    putStrLn "Action validation fail"
    return $ encode es
  (Right args) -> do
    putStrLn "Action validation ok. Start domain action..."
    r <- encode <$> (df args)
    putStrLn "domain action done."
    return r

instance ToJSON ResultOk_ where
  toJSON (_) = Data.Aeson.object ["result" .= ("ok" :: String)]

instance (ToJSON e, ToJSON r) => ToJSON (ErrorOrResult e r) where
  toJSON (DError e) = Data.Aeson.object ["result" .= ("fail" :: String), "errors" .= toJSON e]
  toJSON (DResult r) = toJSON r

instance ToJSON a => ToJSON (FieldRequired a) where
  toJSON FieldRequired = Data.Aeson.String "FieldRequired"
  toJSON (FieldErrors a) = toJSON a

instance ToJSON Email where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EmailValidationError where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON UserName where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON UserNameValidationError where
  toEncoding = genericToEncoding defaultOptions
