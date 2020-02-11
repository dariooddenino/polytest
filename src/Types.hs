module Types where

import           Data.ByteArray                   (ByteArray, ByteArrayAccess)
import           Data.ByteString                  (ByteString)
import           Data.String                      (IsString)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import qualified Polysemy as P
import           Prelude hiding (getLine, putStrLn, putStr)

-- | A few type definitions to get started.
newtype Username = Username ByteString
  deriving (Eq, Ord, Show, ToField, IsString)

newtype Password = Password ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray, IsString)

newtype PasswordHash = PasswordHash ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray, ToField, FromField)



-- | We have to define our effects as a GADT.
-- | `m` is a monad, and `a` is the return type.
-- | The two data constructors correspond to the operations we want to perform.
data CryptoHash m a where
  -- | Generates a hash from a password.
  MakeHash :: Password -> CryptoHash m PasswordHash
  -- | Check if a password matches a hash.
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

-- | These two functions convert a value of type CryptoHash (Sem r) a to a value
-- | of type Sem r a. The member constraint is used to make the effect explicit
-- | without having to manually manage the effects list.

-- makeHash' :: P.Member CryptoHash r => Password -> P.Sem r PasswordHash
-- makeHash' password = PI.send (MakeHash password :: CryptoHash (P.Sem r) PasswordHash)

-- validateHash' :: P.Member CryptoHash r => Password -> PasswordHash -> P.Sem r Bool
-- validateHash' password hash = PI.send (ValidateHash password hash :: CryptoHash (P.Sem r) Bool)

-- | We don't need to manyally write them, as we can use:
P.makeSem ''CryptoHash


data Console m a where
  PutStrLn :: ByteString -> Console m ()
  PutStr   :: ByteString -> Console m ()
  GetLine :: Console m ByteString
P.makeSem ''Console
