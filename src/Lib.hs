module Lib where

import qualified Polysemy         as P
import qualified Polysemy.KVStore as PK
import           Prelude hiding (putStrLn, putStr, getLine)
import           Types


-- | The operations to be implemented by the password manager
addUser ::
  P.Members [CryptoHash, PK.KVStore Username PasswordHash] r =>
  Username ->
  Password ->
  P.Sem r ()
addUser username password = do
  hashedPassword <- makeHash password
  PK.writeKV username hashedPassword

validatePassword ::
  P.Members [CryptoHash, PK.KVStore Username PasswordHash] r =>
  Username ->
  Password ->
  P.Sem r Bool
validatePassword username password = do
  hashInStore <- PK.lookupKV username
  case hashInStore of
    Just h -> validateHash password h
    Nothing -> return False


addUserProgram ::
  P.Members [CryptoHash, PK.KVStore Username PasswordHash, Console] r =>
  P.Sem r ()
addUserProgram = do
  putStrLn "Creating a user."
  putStrLn "Insert a username:"
  u <- getLine
  putStrLn "Insert a password:"
  p <- getLine
  putStrLn $ "Adding " <> u <> " " <> p
  addUser (Username u) (Password p)

validatePasswordProgram ::
  P.Members [CryptoHash, PK.KVStore Username PasswordHash, Console] r =>
  P.Sem r Bool
validatePasswordProgram = do
  putStrLn "Login:"
  putStrLn "Insert the username:"
  u <- getLine
  putStrLn "Insert the password:"
  p <- getLine
  validatePassword (Username u) (Password p)
  -- printResult b
