module Effectful where

import qualified Crypto.KDF.BCrypt      as BCrypt
import qualified Crypto.Random          as CR
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Maybe (listToMaybe)
import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import qualified Polysemy.KVStore       as PK
import qualified Polysemy.State         as PS
import           Prelude hiding (putStrLn, putStr, getLine)
import           Lib
import           Types

runCryptoHashAsState ::
  (CR.DRG gen, P.Member (PS.State gen) r) =>
  P.Sem (CryptoHash : r) a ->
  P.Sem r a
runCryptoHashAsState = P.interpret $ \case
  ValidateHash password hash -> return $ BCrypt.validatePassword password hash
  MakeHash password -> do
    drg <- PS.get
    let (hash, drg') = CR.withDRG drg $ BCrypt.hashPassword 5 password
    PS.put drg'
    return hash

runKVStoreAsSQLite ::
  P.Member (P.Embed IO) r =>
  P.Sem (PK.KVStore Username PasswordHash ': r) a ->
  P.Sem (PI.Input SQL.Connection : r) a
runKVStoreAsSQLite = P.reinterpret $ \case
  PK.LookupKV username -> do
    conn <- PI.input
    hashes <- P.embed $ SQL.queryNamed conn
              "SELECT hash FROM passwords WHERE username = :username"
              [":username" := username]
    return (SQL.fromOnly <$> listToMaybe hashes)
  PK.UpdateKV username maybeHash -> do
    let (query, params) =
          case maybeHash of
            Just hash -> ( "INSERT INTO passwords (username, hash) VALUES (:username, :hash) " <>
                           "ON CONFLICT (username) DO UPDATE SET HASH = excluded.hash"
                        , [":username" := username, ":hash" := hash] )
            Nothing -> ( "DELETE FROM passwords WHERE username = :username"
                       , [":username" := username] )
    conn <- PI.input
    P.embed $ SQL.executeNamed conn query params

runConsoleIO ::
  P.Member (P.Embed IO) r =>
  P.Sem (Console ': r) a ->
  P.Sem r a
runConsoleIO = P.interpret $ \case
  PutStrLn l -> P.embed $ BS.putStrLn l
  PutStr l -> P.embed $ BS.putStr l
  GetLine -> P.embed BS.getLine

runAllEffects ::
  CR.DRG gen =>
  gen ->
  SQL.Connection ->
  (forall r. P.Members [Console, CryptoHash, PK.KVStore Username PasswordHash] r => P.Sem r a) ->
  IO a
runAllEffects drg conn program =
  program                       -- [CryptoHash, KVStore Username PasswordHash]
    & runCryptoHashAsState      -- [KVStore Username PasswordHash, State gen]
    & runKVStoreAsSQLite        -- [Input Connection, State gen, Embed IO]
    & PI.runInputConst conn     -- [State gen, Embed IO]
    & PS.evalState drg          -- [Embed IO]
    & runConsoleIO
    & P.runM

-- Interpret the operations

dbFile :: FilePath
dbFile = "password-manager.db"

withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS passwords (username TEXT PRIMARY KEY, hash TEXT)"
  f conn

runAddUser :: SQL.Connection -> IO ()
runAddUser conn = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn
    addUserProgram

runValidatePassword :: SQL.Connection -> IO ()
runValidatePassword conn = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn $ do
    b <- validatePasswordProgram
    printResult b

    where
      printResult :: P.Member Console r => Bool -> P.Sem r ()
      printResult True = putStrLn "Login successful"
      printResult False = putStrLn "Wrong username and/or password"

runE :: IO ()
runE =
  withPasswordDBConnection $ \conn -> do
    runAddUser conn
    runValidatePassword conn
