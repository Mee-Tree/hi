{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase  #-}

module Hi.Action
  ( HIO (HIO, runHIO)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.List (sort)
import Data.Set (Set, notMember)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import Prelude hiding (read)
import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.Random.Stateful (getStdRandom, uniformR)

import Hi.Base (HiAction (..), HiMonad, HiValuable (returnValue, toValue), HiValue (..), runAction)

import qualified Data.ByteString as B (readFile, writeFile)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)


data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Enum, Bounded)

instance Show HiPermission where
  show = \case
    AllowRead  -> "read"
    AllowWrite -> "write"
    AllowTime  -> "time"

data PermissionException = PermissionRequired HiPermission
  deriving (Eq)

instance Show PermissionException where
  show (PermissionRequired p) = show p ++ " permission required"

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction = \case
    HiActionRead file        -> read file
    HiActionWrite file bytes -> write file bytes
    HiActionMkDir path       -> mkdir path
    HiActionChDir path       -> cd path
    HiActionRand from to     -> rand from to
    HiActionEcho text        -> echo text
    HiActionCwd              -> cwd
    HiActionNow              -> now

-- | Lifts the provided 'IO' to 'HIO' if the given permission is present
--   in the permission set, throws 'PermissionRequired' otherwise.
with :: HiPermission -> IO a -> HIO a
with p io = HIO $ \perms -> do
  when (p `notMember` perms) $ throwIO (PermissionRequired p)
  io

-- | Returns the canonical form of the provided path.
canonicalize :: FilePath -> IO FilePath
canonicalize path = do
  curDir <- getCurrentDirectory
  canonicalizePath (curDir </> path)

-- | Returns a list of all entries in the provided file if it is a directory,
--   and the contents of it otherwise. The contents are returned using
--   either 'HiValueString' or 'HiValueBytes' based on whether they are valid /UTF-8/.
--   Requires 'AllowRead' permission.
read :: FilePath -> HIO HiValue
read path = with AllowRead $ do
  newPath <- canonicalize path
  isDir <- doesDirectoryExist newPath
  if isDir then do
    dirs <- sort <$> listDirectory newPath
    returnValue (toValue <$> dirs)
  else do
    contents <- B.readFile newPath
    let result = fromRight (HiValueBytes contents) $
          toValue <$> decodeUtf8' contents
    return result

-- | Writes a 'ByteString' to the provided file. Requires 'AllowWrite' permission.
write :: FilePath -> ByteString -> HIO HiValue
write path bytes = with AllowWrite $ do
  newPath <- canonicalize path
  B.writeFile newPath bytes
  return HiValueNull

-- | Creates a new directory with the given path. Requires 'AllowWrite' permission.
mkdir :: FilePath -> HIO HiValue
mkdir dir = with AllowWrite $ do
  newDir <- canonicalize dir
  createDirectory newDir
  return HiValueNull

-- | Changes the current working directory to the given path.
--   Requires 'AllowRead' permission.
cd :: FilePath -> HIO HiValue
cd path = with AllowRead $ do
  newDir <- canonicalize path
  setCurrentDirectory newDir
  return HiValueNull

-- | Returns the current working directory. Requires 'AllowRead' permission.
cwd :: HIO HiValue
cwd = with AllowRead $ do
  toValue . T.pack <$> getCurrentDirectory

-- | Returns the current system time. Requires 'AllowTime' permission.
now :: HIO HiValue
now = with AllowTime $ do
  toValue <$> getCurrentTime

-- | Returns the pseudorandom, uniformly distributed 'Int' value over the provided range.
rand :: Int -> Int -> HIO HiValue
rand from to = do
  rnd <- getStdRandom (uniformR (from, to))
  returnValue rnd

-- | Prints the given 'Text' followed by a newline to the stdout.
--   Requires 'AllowWrite' permission.
echo :: Text -> HIO HiValue
echo str = with AllowWrite $ do
  HiValueNull <$ T.putStrLn str
