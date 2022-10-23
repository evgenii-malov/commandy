{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Users.Mapper where

-- need to print params! ??
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as CHLB
import Data.List
import Data.Text (pack, unpack, splitOn)
import Users.Types
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import qualified TH.ReifySimple as RS
import qualified Data.Commandy.Types as CT
import Data.Aeson
import Data.Either
import Data.Maybe
import qualified Data.Commandy.Json as CJS
import qualified Data.Aeson.Key as AK
import qualified Data.Map as M
import qualified Commandy as CD
-- need mapping from "Data.Commandy.Types.Email" => makeEmail
-- (ToJSON de, ToJSON dr) =>  

-- ex_ d k = ((fromRight undefined) . CT.fromString) <$> CJS.exF d k
-- ex d k = fromJust $ ((fromRight undefined) . CT.fromString) <$> CJS.exF d k

-- TODO -- do real maybe check



ismaybe :: Type -> Bool
ismaybe(AppT (ConT _) _) = True
ismaybe(ConT _) = False
ismaybe(_) = error "unexpected types configuration: use Maybe a | a" 

extr (AppT (ConT _) t) = t
extr t@(ConT _) = t
extr _ = error "unexpected type conf in EXTR"

tname (ConT n) = show n
tname (_) = error "unexpected type cons"


-- TODO: this explicit mapping can be removed, all info containt inside typeclass instances of fromstring
-- TODO search for instances fromString and build mapping from fieldtype => errorFtype
tMapping = M.fromList [
            ("Data.Commandy.Types.Email", ConT $ mkName "Data.Commandy.Types.EmailValidationError"),
            ("Data.Commandy.Types.UserName", ConT $ mkName "Data.Commandy.Types.UserNameValidationError")
          ]


-- eex_ = [e| \d k -> ((fromRight undefined) . CT.fromString) <$> (exF d k) |]
-- eex = [e| \d k -> fromJust $ ((fromRight undefined) . CT.fromString) <$> (exF d k) |]  

srouter :: String -> Name -> Name -> Name -> Name -> Q [Dec]
srouter name rc r e dact = do
  -- https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/src/Language.Haskell.TH.Syntax.html#TyConI
  tr@(TyConI (DataD _ n _ _ [constr@(RecC cn varbangtype)] _)) <- reify r
  let args_and_types = [(unpack $ last $ splitOn "." $ pack . show $ n, extr tn, ismaybe tn) | (n, _, tn) <- varbangtype]
  te@(TyConI (DataD _ n _ _ [(RecC cn varbangtype)] _)) <- reify e
  dact_ <- reify dact
  rc_ <- reify rc
  
  -- let [(n1,_,n1t),(n2,_,n2t)] = varbangtype
  
  --let e_args_and_types = [(last $ splitOn "." $ pack . show $ n, extr tn, ismaybe tn) | (n, _, tn) <- varbangtype]
  
  -- te@(StarT) <- reifyType e
  -- te <- reify e
  -- return $ trace ("tr = " ++ show tr) ()

  eex_ <- [e| \d k -> ((fromRight undefined) . CT.fromString) <$> (exF d k) |]
  eex <- [e| \d k -> fromJust $ ((fromRight undefined) . CT.fromString) <$> (exF d k) |]

  -- exv_ <- [e| \d k -> ((CT.fromString)) <$> (exF d k) |]
  -- exv <- [e| \d k -> fromJust $ (CT.fromString) <$> (exF d k) |]

  exv <- [e| \d k -> ((CT.fromString)) <$> (exF d k) |]
  
  
  --let apply_cons = \c args_types rdt -> [e| foldl (AppE) (ConE c) [  (AppE (AppE (if fns == "email" then  eex else eex_ ) rdt ) (LitE $ StringL fns))  | (fns,ft) <- args_types] |]
  --apply_cons <- [e| \c args_types rdt -> foldl (AppE) (ConE c) [  (AppE (AppE (if fns == "email" then  eex else eex_ ) rdt ) (LitE $ StringL fns))  | (fns,ft) <- args_types] |]

  d <-
    [d|
      rt :: LB.ByteString -> IO LB.ByteString
      rt rdata =
        do
          --let x = 1 :: Int
          --print d
          --RegArgs
          -- $(ConT dact)
          -- $([t|ConT dact|])  - extract Data Cons ?
          -- $([e| constr |])
          -- $(return $ RecConE r [])
          -- lookupTypeName ??

          -- putStrLn $ "injected!" ++ (nameBase cn) ++ (nameBase n2) ++ (show n2t) ++ (show te)
          putStrLn $ "injected!" ++ (show args_and_types) ++ "\n" ++  "\n" ++ (show dact_)
          --putStrLn $ "---1"
          --putStrLn $ (show rc_)
          --putStrLn $ "---2"
          -- extract fields values from jsom
          -- apply SmartConstructors to fields
          -- check any errors
          -- if no arrors build args
          -- if error build errors

          -- let p1 = ((fromRight undefined) . CT.fromString <$> exF rdata "username" ) -- :: Maybe CT.UserName
          --let p2 = (fromJust ((fromRight undefined) . CT.fromString <$> exF rdata "email")) -- :: CT.Email          
            -- let p1 = ex_ rdata "username"
            -- let p2 = ex rdata "email"
          -- $(LetS [ValD (VarP (mkName "p1")) (NormalB [e| ex rdata "username" |])])
          

          -- $(return $ )
          --let fnames = ["p1","p2"]          
          -- make AST, wrap it into Q, splice with $ and bind name args to constructed code
          --foldl (AppE) (ConE rc) [VarE (mkName "p1"),VarE (mkName "p2")]
          --let args = $(return $ AppE (AppE (ConE rc) $ VarE (mkName "p1")) $ VarE (mkName "p2"))
          
          -- TODO !!! REMOVE VarE (mkName fns) REPLACE WITH AppE (AppE $ (VarE (mkName ex)) rdata) VarE (mkName fns)  "username"
          -- let args = $(return $ foldl (AppE) (ConE rc) [VarE (mkName fns) | fns <- ["p1","p2"]])
          
          --let args = $(return $ foldl (AppE) (ConE rc) [ SigE (ParensE (AppE (AppE (if fns == "email" then  (VarE (mkName "ex")) else (VarE (mkName "ex_")) ) (VarE (mkName "rdata")) ) (LitE $ StringL fns))) $ if fns == "username" then AppT (ConT (mkName "Maybe")) (ConT (mkName ft) ) else (ConT (mkName ft))  | (fns,ft) <- [("username","Data.Commandy.Types.UserName"),("email","Data.Commandy.Types.Email")]])
          
          --let args = $(return $ foldl (AppE) (ConE rc) [ SigE (ParensE (AppE (AppE (if fns == "email" then  eex else eex_ ) (VarE (mkName "rdata")) ) (LitE $ StringL fns))) $ if fns == "username" then AppT (ConT (mkName "Maybe")) (ConT (mkName ft) ) else (ConT (mkName ft))  | (fns,ft) <- [("username" ,"Data.Commandy.Types.UserName"),("email" ,"Data.Commandy.Types.Email")]])
          
          --let hasErrs = any id [if ismaybe ft then e_ $ (CT.fromString <$> exF rdata k) else e $ (CT.fromString <$> exF rdata k) | (k,ft) <- args_and_types]

                    
          let args = $(let 
                          ce = ConE rc 
                          barg fns ismb = (AppE (AppE (if ismb then eex_ else eex ) (VarE (mkName "rdata")) ) (LitE $ StringL fns))                        
                        in 
                         return $ foldl (AppE) ce [ barg fns ismb | (fns, _, ismb) <- args_and_types])

                  

          let is_verrors = $(let 
                              ff = VarE $ mkName $ "||"
                              barg fns = (AppE (AppE exv (VarE (mkName "rdata")) ) (LitE $ StringL fns))
                              --dsig e ft = SigE e $ (AppT (AppT (ConT ''Either) (AppT ListT ((tMapping M.! (tname ft) )))) ft)
                              dsig_ e ft = SigE e $ AppT (ConT ''Maybe) (AppT (AppT (ConT ''Either) (AppT ListT ((tMapping M.! (tname ft))))) ft)                                                            
                              chose_e ft = if ismaybe ft then (VarE (mkName "e_")) else (VarE (mkName "e"))
                            in 
                              --return $ foldl (AppE) ff [ (if ismaybe ft then AppE (VarE (mkName "e_")) $ dsig_ (barg fns) ft else AppE (VarE (mkName "e")) $ dsig_ (barg fns) ft) | (fns,ft,ismb) <- args_and_types])
                              return $ foldl (AppE) ff [ (AppE (chose_e ft) $ dsig_ (barg fns) ft) | (fns,ft,ismb) <- args_and_types])




          

          
          -- let args = $(return [|apply_cons rc args_and_types (VarE (mkName "rdata"))|])
          -- let args = $(apply_cons rc args_and_types (VarE (mkName "rdata")))


          --let args = $(return $  (foldl (AppE) (ConE rc)  [ (AppE (AppE (if fns == "email" then  (VarE (mkName "ex")) else (VarE (mkName "ex_")) ) (VarE (mkName "rdata")) ) (LitE $ StringL fns))  | (fns,ft) <- [("username","Data.Commandy.Types.UserName"),("email","Data.Commandy.Types.Email")]]) )
          
          
          -- SigE (VarE (mkName "ex_")) $ VarT (mkName "UserName")
          --let args = $(return $ foldl (AppE) (ConE rc) [$([| ex |]) rdata "username" | fns <- ["p1","p2"]])
          putStrLn $ show is_verrors 
          
          router_ (Right args) $(return $ (VarE dact))
          --router_ (Right $(return $ VarE (mkName "args") )) $(return $ (VarE dact))
                                 
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