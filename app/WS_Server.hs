{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Commandy
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad (forever)
import Data.Aeson
import qualified Data.Aeson.Key as LB
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import Data.Commandy.Json
import Data.Commandy.Types
import Data.Either (fromLeft, fromRight, isLeft)
import qualified Data.Either.Combinators as EithQ
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding
import GHC.Generics
import qualified Network.WebSockets as WS
import Users.Args
import Users.Json
import Users.Types
import Users.Mapper
import qualified Data.Commandy.Types as CT

jsonStringAct :: LB.ByteString
jsonStringAct = "{ \"action\": \"reg\", \"username\": \"John\" , \"email\": \"john@gmail.com\" }"

jsonStringAct2 = "{ \"action\": \"reg\", \"username\": \"John\" , \"email\": \"john!gmail.com\" }" :: LB.ByteString

-- re = RegArgsErrors {username_error = [], email_error = FieldRequired}
-- re2 = RegArgsErrors {username_error = [], email_error = FieldErrors [MustContainAt, MustNotBeEmpty]}

-- !!! catch parse error !!!

exF' :: FromJSON b => LB.ByteString -> Key -> Either String b
exF' t n = do
  result <- eitherDecode t -- "{\"name\":\"Dave\",\"age\":2}"
  flip parseEither result $ \obj -> obj .: n

$(srouter "regRouter" 'RegArgs ''RegArgs ''RegArgsErrors 'reg)

router :: LB.ByteString -> IO LB.ByteString
router b = case act of
  Right "reg" -> do -- $(srouter ''RegArgs ''RegArgsErrors) b
                    regRouter b
                    --router_ (parseCmdfromJson b) reg
  Right act -> do
    putStrLn "recieved uncknown act!"
    return $ "uncknown act " `mappend` LBU8.fromString act
  Left err -> do
    putStrLn "recieved bad json!"
    return $ "error: " `mappend` LBU8.fromString err
  where
    (act :: Either String String) = exF' b "action"

chandle :: WS.Connection -> IO ()
chandle conn = forever $ do
  msg <- WS.receiveData conn
  -- catch any exception here
  -- https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
  res <- try $ router msg :: IO (Either SomeException LB.ByteString)
  case res of
    Left ex -> do
      let excinf = "Caught exception: " ++ show ex
      putStrLn $ excinf
      WS.sendTextData conn $ LBU8.fromString excinf
    Right resp ->
      do
        putStrLn "start send to client..."
        WS.sendTextData conn resp
        putStrLn "done."

-- WS.sendTextData conn $ msg `T.append` ", handle"
-- WS.sendTextData conn resp

main =
  print "serving 8080"
    >> ( WS.runServer "127.0.0.1" 8080 $ \pending -> do
           conn <- WS.acceptRequest pending
           print "accept conn"
           chandle conn
       )

-- ws://127.0.0.1:8080
-- { "action": "reg", "username": "John" , "email": "john@gmail.com" }
-- https://codereview.stackexchange.com/questions/200310/using-mysql-with-wai-and-warp
-- https://hackage.haskell.org/package/resource-pool-0.3.1.0/docs/Data-Pool.html

-- https://begriffs.com/posts/2014-10-25-creating-package-hackage.html
-- https://cabal.readthedocs.io/en/3.4/developing-packages.html

-- logging
-- env vars https://hackage.haskell.org/package/load-env
-- polling
-- ddos