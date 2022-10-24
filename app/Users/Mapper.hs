{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Users.Mapper where

-- need to print params! ??

import qualified Commandy as CD
import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as CHLB
import qualified Data.Commandy.Json as CJS
import qualified Data.Commandy.Types as CT
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (pack, splitOn, unpack)
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as THS
import qualified TH.ReifySimple as RS
import Users.Types

-- TODO check real maybe
ismaybe :: Type -> Bool
ismaybe (AppT (ConT _) _) = True
ismaybe (ConT _) = False
ismaybe (_) = error "unexpected types configuration: use Maybe a | a"

extr (AppT (ConT _) t) = t
extr t@(ConT _) = t
extr _ = error "unexpected type conf in EXTR"

tname (ConT n) = show n
tname (_) = error "unexpected type cons"

-- TODO: this explicit mapping can be removed, all info containt inside typeclass instances of fromstring
-- TODO search for instances fromString and build mapping from fieldtype => errorFtype
tMapping =
  M.fromList
    [ ("Data.Commandy.Types.Email", ConT $ mkName "Data.Commandy.Types.EmailValidationError"),
      ("Data.Commandy.Types.UserName", ConT $ mkName "Data.Commandy.Types.UserNameValidationError"),
      ("Data.Commandy.Types.Password", ConT $ mkName "Data.Commandy.Types.PasswordValidationError")
    ]

-- eex_ = [e| \d k -> ((fromRight undefined) . CT.fromString) <$> (exF d k) |]
-- eex = [e| \d k -> fromJust $ ((fromRight undefined) . CT.fromString) <$> (exF d k) |]

x_Errs :: Type -> Q Exp
x_Errs ft = [e|\d k -> errs $ ((CT.fromString)) ::$ (ft) <$> (exF d k)|]

x_Errs_ :: Type -> Q Exp
x_Errs_ ft = [e|\d k -> errs_ $ ((CT.fromString)) ::$ (ft) <$> (exF d k)|]

srouter :: String -> Name -> Name -> Q [Dec]
srouter name r dact = do
  -- https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/src/Language.Haskell.TH.Syntax.html#TyConI
  tr@(TyConI (DataD _ n _ _ [constr@(RecC cn varbangtype)] _)) <- reify r
  let args_and_types = [(unpack $ last $ splitOn "." $ pack . show $ n, extr tn, ismaybe tn) | (n, _, tn) <- varbangtype]

  eex_ <- [e|\d k -> ((fromRight undefined) . CT.fromString) <$> (exF d k)|]
  eex <- [e|\d k -> fromJust $ ((fromRight undefined) . CT.fromString) <$> (exF d k)|]

  exv <- [e|\d k -> ((CT.fromString)) <$> (exF d k)|]

  d <-
    [d|
      -- rt :: LB.ByteString -> Either Value RegArgs
      rt :: LB.ByteString -> IO LB.ByteString
      rt rdata =
        do
          -- let x = 1 :: Int
          -- print d
          -- RegArgs
          -- \$(ConT dact)
          -- \$([t|ConT dact|])  - extract Data Cons ?
          -- \$([e| constr |])
          -- \$(return $ RecConE r [])
          -- lookupTypeName ??

          -- putStrLn $ "injected!" ++ (nameBase cn) ++ (nameBase n2) ++ (show n2t) ++ (show te)
          -- putStrLn $ "injected!" ++ (show args_and_types) ++ "\n" ++  "\n" ++ (show dact_)
          -- putStrLn $ "---1"
          -- putStrLn $ (show rc_)
          -- putStrLn $ "---2"
          -- extract fields values from jsom
          -- apply SmartConstructors to fields
          -- check any errors
          -- if no arrors build args
          -- if error build errors
          -- let fnames = ["p1","p2"]
          -- make AST, wrap it into Q, splice with $ and bind name args to constructed code

          let args =
                $( let ce = ConE $ cn
                       barg fns ismb = (AppE (AppE (if ismb then eex_ else eex) (VarE 'rdata)) (LitE $ StringL fns))
                    in return $ foldl (AppE) ce [barg fns ismb | (fns, _, ismb) <- args_and_types]
                 )

          let is_verrors =
                $( let ff = VarE '(||)
                       barg fns = (AppE (AppE exv (VarE 'rdata)) (LitE $ StringL fns))
                       dsig_ e ft = SigE e $ AppT (ConT ''Maybe) (AppT (AppT (ConT ''Either) (AppT ListT ((tMapping M.! (tname ft))))) ft)
                       chose_e ft = if ismaybe ft then (VarE 'CD.e_) else (VarE 'CD.e)
                    in return $ foldl (AppE) ff [(AppE (chose_e ft) $ dsig_ (barg fns) ft) | (fns, ft, ismb) <- args_and_types]
                 )

          -- putStrLn $ show is_verrors

          let errors_value =
                $( let -- build json value (toJson)
                       -- buildt ft ismb = if ismb then (AppT ListT ((tMapping M.! (tname ft)))) else AppT (ConT ''CD.FieldRequired) (AppT ListT ((tMapping M.! (tname ft))))
                       -- berrs fns ismb ft = SigE (AppE (AppE (if ismb then ex_errs_ else ex_errs ) (VarE 'rdata) ) (LitE $ StringL fns)) $ buildt ft ismb

                       barg fns = (AppE (AppE exv (VarE 'rdata)) (LitE $ StringL fns))
                       dsig_ e ft = SigE e $ AppT (ConT ''Maybe) (AppT (AppT (ConT ''Either) (AppT ListT ((tMapping M.! (tname ft))))) ft)
                       chose_e ft = if ismaybe ft then (VarE 'CD.errs_) else (VarE 'CD.errs)
                    in -- errs fn ismb rdata = AppE (VarE 'toJSON) (SigE (LitE $ StringL "some errors") $ ConT ''String)
                       return $ AppE (VarE 'object) $ ListE [AppE (AppE (VarE '(.=)) (LitE (StringL fn))) (AppE (chose_e ft) $ dsig_ (barg fn) ft) | (fn, ft, ismb) <- args_and_types]
                 )
          -- let erros_value = toJSON $ ("123" :: String)
          let wrapped_errors_value =
                object
                  [ "result" .= ("fail" :: String),
                    "data" .= errors_value
                  ]

          -- if not is_verrors then router_ (Right args) $(return $ (VarE dact)) else return $ encode wrapped_errors_value
          if not is_verrors
            then do
              putStrLn "Action validation ok. Start domain action..."
              (encode <$> ($(return (VarE dact)) args)) >>= \r -> print "Domain action done!" >> return r
            else return $ encode wrapped_errors_value

            -- {-# LANGUAGE OverloadedStrings #-}
            -- :set -XOverloadedStrings
            -- object :: [Pair] -> Value

            -- customValue :: Value
            -- customValue = object
            --   [ "list_price" .= (150000 :: Int)
            --   , "sale_price" .= (143000 :: Int)
            --   , "description" .= ("2-bedroom townhouse" :: String)
            --   ]
      |]

  let funName = mkName name
      [SigD _ funSig, FunD _ funBody] = d
      d' = [SigD funName funSig, FunD funName funBody]
  return d'

-- hasErrors :: LB.ByteString -> [(Key, FieldType, SC)] -> Bool
-- hasErrors t fields = any id [if ft == FNull then e_ $ pf t fn sc else e $ pf t fn sc | (fn, ft, (SC sc)) <- fields]

-- if is_errors -- hasErrors t regFields
--   then
--     Left $
--       RegArgsErrors
--         (gErrs_ t $ _username regFields_) -- (errs_ username_)
--         (gErrs t $ _email regFields_) -- (errs email_)
--   else
--     Right $
--       RegArgs
--         (gRes_ t $ _username regFields_) -- (res_ username_)
--         (gRes t $ _email regFields_) -- (res email_)
-- where
--   -- is_errors = e username_ || e_ email_
--   is_errors = (isFerr t $ _username regFields_) || (isFerr t $ _email regFields_)

-- https://hackage.haskell.org/package/modulespection

-- make or in AST: VarE $ mkName $ "GHC.Classes.||"
-- make type in AST: ConT $ mkName "GHC.Maybe.Maybe"
-- ConT $ ''Either