module Pure where

import qualified Polysemy as P
import qualified Polysemy.KVStore as PK
import Lib
import Types
import Data.Function ((&))
import Prelude hiding (putStrLn, putStr, getLine)

-- | This pure version doesn't hash the password.
runCryptoHashPure ::
  P.Sem (CryptoHash : r) a ->
  P.Sem r a
runCryptoHashPure = P.interpret $ \case
  ValidateHash (Password p) (PasswordHash h) -> return $ p == h
  MakeHash (Password p) -> return $ PasswordHash p

runKVStorePure ::
  P.Sem (PK.KVStore Username PasswordHash : r) a ->
  P.Sem r a
runKVStorePure = P.interpret $ \case
  PK.LookupKV _ -> return $ Just (PasswordHash "foo")
  PK.UpdateKV _ _ -> return ()

runConsolePure ::
  P.Sem (Console : r) a ->
  P.Sem r a
runConsolePure = P.interpret $ \case
  PutStrLn _ -> return ()
  PutStr _ -> return ()
  GetLine -> return "foo"

runAllPure ::
  (forall r. P.Members [Console, CryptoHash, PK.KVStore Username PasswordHash] r => P.Sem r a) ->
  a
runAllPure program =
  program                   -- [CryptoHash, KVStore Username PasswordHash, Console]
    & runCryptoHashPure     -- [KVstore Username PasswordHash, Console]
    & runKVStorePure        -- [Console]
    & runConsolePure        -- []
    & P.run

runAddUser :: ()
runAddUser = runAllPure addUserProgram

runValidatePassword :: Bool
runValidatePassword = runAllPure validatePasswordProgram

runP :: Bool
runP =
  runAddUser & const runValidatePassword
