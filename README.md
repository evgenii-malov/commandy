# Commandy


Commandy is simple Web framework written in Haskell

- define your domain commands as types
- define your domain commands results as types
- define all your fields with smart constructor validation
- implicit mapping of json objects to command types (via template haskell mapper)
- auto generation of all errors or success result
- easy to change internal transport (websocket used)
- easy to change command data representation (json, grpc, row text, etc)


Here's an example:

```haskell

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
  let uname = fromMaybe "NULL" (show <$> username)
  let qargs = [show email, "16", uname]
  conn <- connect defaultConnectInfo {connectPassword = "root", connectDatabase = "test"}
  do
    r <- (try $ execute conn ("insert into user (email,age,name) values (?,?,?)") qargs) :: IO (Either MySQLError Int64)
    case r of
      Left e -> if errNumber e == 1062 then return . DError $ EmailUsed email else error "something WRONG!"
      Right _ -> do uid <- insertID conn; print $ "uid" ++ show uid; return $ DResult ROK_

```




This is draft for now!
