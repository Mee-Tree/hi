{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Hi.Base
  ( HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad (..)
  , HasHiValue (..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map, mapKeys)
import Data.Ratio ((%))
import Data.Sequence (Seq, fromList)
import Data.Text (Text, pack, singleton)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


-- | Function names (div, sort, length, ...).
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Enum, Bounded)
  deriving stock (Generic)
  deriving anyclass (Serialise)

instance Show HiFun where
  show = \case
    HiFunDiv            -> "div"
    HiFunMul            -> "mul"
    HiFunAdd            -> "add"
    HiFunSub            -> "sub"
    HiFunNot            -> "not"
    HiFunAnd            -> "and"
    HiFunOr             -> "or"
    HiFunLessThan       -> "less-than"
    HiFunGreaterThan    -> "greater-than"
    HiFunEquals         -> "equals"
    HiFunNotLessThan    -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals      -> "not-equals"
    HiFunIf             -> "if"
    HiFunLength         -> "length"
    HiFunToUpper        -> "to-upper"
    HiFunToLower        -> "to-lower"
    HiFunReverse        -> "reverse"
    HiFunTrim           -> "trim"
    HiFunList           -> "list"
    HiFunRange          -> "range"
    HiFunFold           -> "fold"
    HiFunPackBytes      -> "pack-bytes"
    HiFunUnpackBytes    -> "unpack-bytes"
    HiFunEncodeUtf8     -> "encode-utf8"
    HiFunDecodeUtf8     -> "decode-utf8"
    HiFunZip            -> "zip"
    HiFunUnzip          -> "unzip"
    HiFunSerialise      -> "serialise"
    HiFunDeserialise    -> "deserialise"
    HiFunRead           -> "read"
    HiFunWrite          -> "write"
    HiFunMkDir          -> "mkdir"
    HiFunChDir          -> "cd"
    HiFunParseTime      -> "parse-time"
    HiFunRand           -> "rand"
    HiFunEcho           -> "echo"
    HiFunCount          -> "count"
    HiFunKeys           -> "keys"
    HiFunValues         -> "values"
    HiFunInvert         -> "invert"

-- | Values (numbers, booleans, strings, ...).
data HiValue
  = HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueString Text
  | HiValueBytes ByteString
  | HiValueList (Seq HiValue)
  | HiValueFunction HiFun
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Expressions (literals, function calls, ...).
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

-- | Evaluation errors (invalid arguments, ...).
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq)

instance Show HiError where
  show = \case
    HiErrorInvalidArgument -> "invalid argument"
    HiErrorInvalidFunction -> "invalid function"
    HiErrorArityMismatch   -> "arity mismatch"
    HiErrorDivideByZero    -> "division by zero"

-- | Actions (read, write, ...).
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionRand Int Int
  | HiActionEcho Text
  | HiActionCwd
  | HiActionNow
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

class HasHiValue a where
  toValue :: a -> HiValue

  returnValue :: Monad m => a -> m HiValue
  returnValue = return . toValue

instance HasHiValue Bool where
  toValue = HiValueBool

instance HasHiValue Rational where
  toValue = HiValueNumber

instance {-# OVERLAPPABLE #-} Integral a => HasHiValue a where
  toValue = toValue . (% 1) . toInteger

instance HasHiValue Text where
  toValue = HiValueString

instance HasHiValue String where
  toValue = toValue . pack

instance HasHiValue Char where
  toValue = toValue . singleton

instance HasHiValue ByteString where
  toValue = HiValueBytes

instance HasHiValue a => HasHiValue (Seq a) where
  toValue = HiValueList . (toValue <$>)

instance {-# OVERLAPPABLE #-} HasHiValue a => HasHiValue [a] where
  toValue = toValue . fromList

instance HasHiValue HiFun where
  toValue = HiValueFunction

instance HasHiValue HiAction where
  toValue = HiValueAction

instance HasHiValue UTCTime where
  toValue = HiValueTime

instance (HasHiValue k, HasHiValue v) => HasHiValue (Map k v) where
  toValue = HiValueDict . (toValue <$>) . (mapKeys toValue)

instance HasHiValue HiValue where
  toValue = id
