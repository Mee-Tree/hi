{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Hi.Evaluator
  ( eval
  ) where

import Prelude hiding (drop, null, take)

import Codec.Serialise (DeserialiseFailure, Serialise)
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT, MonadError, lift, runExceptT, throwError)
import Data.Bitraversable (bimapM)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char (chr, ord)
import Data.Either (fromRight)
import Data.Foldable (Foldable (toList))
import Data.ListLike (ListLike)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Text.Read (readMaybe)

import Hi.Base (HasHiValue (..),
                HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..))

import qualified "pure-zlib" Codec.Compression.Zlib as Z.P (DecompressionError, decompress)
import qualified "zlib" Codec.Compression.Zlib as Z (CompressParams (compressLevel),
                                                     bestCompression, compressWith,
                                                     defaultCompressParams)
import qualified Codec.Serialise as C (deserialiseOrFail, serialise)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ListLike as L (drop, empty, index, length, reverse, take)
import qualified Data.Map as M (elems, fromList, fromListWith, keys, lookup, toList)
import qualified Data.Text as T (strip, toLower, toUpper, unpack)


between :: (Ord a) => a -> a -> a -> Bool
between l r x = l <= x && x <= r

integer :: Rational -> Bool
integer x = denominator x == 1

int :: Rational -> Bool
int x = integer x && between mnB mxB x
  where mnB = toRational (minBound :: Int)
        mxB = toRational (maxBound :: Int)

-- | Evaluates the given expression.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

-- | Helper for 'eval'.
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue value)  = return value
eval' (HiExprApply fun args) = do
  efun <- eval' fun
  case efun of
    HiValueFunction f -> evalLazyFun f args
    HiValueString s   -> forM args eval' >>= indexOrSlice s
    HiValueBytes b    -> forM args eval' >>= indexOrSlice b
    HiValueList l     -> forM args eval' >>= indexOrSlice l
    HiValueDict d     -> forM args eval' >>= dictLookup d
    _                 -> throwError HiErrorInvalidFunction
eval' (HiExprRun expr) = do
  eexpr <- eval' expr
  case eexpr of
    HiValueAction a -> lift $ runAction a
    _               -> throwError HiErrorInvalidArgument
eval' (HiExprDict pairs) = do
  epairs <- forM pairs (bimapM eval' eval')
  returnValue $ M.fromList epairs

-- | Tries to evaluate the given function lazily, if this function
--   is not lazy falls back to 'evalFun'.
evalLazyFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalLazyFun HiFunIf [cond, true, false] = eval' cond >>= \case
  HiValueBool b -> eval' $ if b then true else false
  _             -> throwError HiErrorInvalidArgument
evalLazyFun HiFunAnd [a, b] = eval' a >>= \case
  bool@(HiValueBool False) -> return bool
  null@HiValueNull         -> return null
  _                        -> eval' b
evalLazyFun HiFunOr [a, b] = eval' a >>= \case
  HiValueBool False -> eval' b
  HiValueNull       -> eval' b
  other             -> return other
evalLazyFun fun args =
  if fun `elem` [HiFunIf, HiFunAnd, HiFunOr] then throwError HiErrorArityMismatch
  else forM args eval' >>= evalFun fun

-- | Evaluates the provided function.
evalFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalFun HiFunEquals [a, b]         = returnValue $ a == b
evalFun HiFunNotEquals [a, b]      = returnValue $ a /= b
evalFun HiFunLessThan [a, b]       = returnValue $ a < b
evalFun HiFunGreaterThan [a, b]    = returnValue $ a > b
evalFun HiFunNotLessThan [a, b]    = returnValue $ a >= b
evalFun HiFunNotGreaterThan [a, b] = returnValue $ a <= b
evalFun HiFunDiv [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> if y /= 0 then returnValue $ x / y
                                        else throwError HiErrorDivideByZero
  (HiValueString x, HiValueString y) -> returnValue $ x <> "/" <> y
  _                                  -> throwError HiErrorInvalidArgument
evalFun HiFunMul [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> returnValue (x * y)
  (HiValueString x, HiValueNumber y) -> y `times` x
  (HiValueList x, HiValueNumber y)   -> y `times` x
  (HiValueBytes x, HiValueNumber y)  -> y `times` x
  _                                  -> throwError HiErrorInvalidArgument
  where
    times :: (HasHiValue a, Semigroup a, MonadError HiError m) => Rational -> a -> m HiValue
    times n x = if integer n && n > 0 then returnValue $ numerator n `stimes` x
                else throwError HiErrorInvalidArgument
evalFun HiFunAdd [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> returnValue $ x + y
  (HiValueString x, HiValueString y) -> returnValue $ x <> y
  (HiValueList x, HiValueList y)     -> returnValue $ x <> y
  (HiValueBytes x, HiValueBytes y)   -> returnValue $ x <> y
  (HiValueTime x, HiValueNumber y)   -> returnValue $ addUTCTime (fromRational y) x
  _                                  -> throwError HiErrorInvalidArgument
evalFun HiFunSub [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> returnValue $ x - y
  (HiValueTime x, HiValueTime y)     -> returnValue $ toRational $ diffUTCTime x y
  _                                  -> throwError HiErrorInvalidArgument
evalFun HiFunNot [a] = case a of
  (HiValueBool x) -> returnValue $ not x
  _               -> throwError HiErrorInvalidArgument
evalFun HiFunLength [a] = case a of
  (HiValueString x) -> returnValue $ L.length x
  (HiValueList x)   -> returnValue $ L.length x
  (HiValueBytes x)  -> returnValue $ L.length x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunToUpper [a] = case a of
  (HiValueString x) -> returnValue $ T.toUpper x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunToLower [a] = case a of
  (HiValueString x) -> returnValue $ T.toLower x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunReverse [a] = case a of
  (HiValueString x) -> returnValue $ L.reverse x
  (HiValueList x)   -> returnValue $ L.reverse x
  (HiValueBytes x)  -> returnValue $ L.reverse x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunTrim [a] = case a of
  (HiValueString x) -> returnValue $ T.strip x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunList xs = returnValue xs
evalFun HiFunRange [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y)     -> returnValue [x .. y]
  (HiValueBool x, HiValueBool y)         -> returnValue [x .. y]
  (HiValueFunction x, HiValueFunction y) -> returnValue [x .. y]
  _                                      -> throwError HiErrorInvalidArgument
evalFun HiFunFold [f, a] = case a of
  (HiValueList x)   -> fold $ toList x
  (HiValueString x) -> fold $ toValue <$> T.unpack x
  (HiValueBytes x)  -> fold $ toValue . ord <$> B.unpack x
  _                 -> throwError HiErrorInvalidArgument
  where
    apply :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    apply x y = eval' (HiExprApply (HiExprValue f) $ HiExprValue <$> [x, y])

    fold :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
    fold (x : xs) = foldM apply x xs
    fold []       = return a
evalFun HiFunPackBytes [a] = case a of
  (HiValueList x) -> do
    ints <- mapM fromNumber (toList x)
    bytes <- mapM byte ints
    returnValue $ B.pack bytes
  _               -> throwError HiErrorInvalidArgument
  where
    fromNumber :: MonadError HiError m => HiValue -> m Rational
    fromNumber (HiValueNumber n) = return n
    fromNumber _                 = throwError HiErrorInvalidArgument

    byte :: MonadError HiError m => Rational -> m Char
    byte x = if integer x && between 0 255 x then return $ chr (fromEnum x)
             else throwError HiErrorInvalidArgument
evalFun HiFunUnpackBytes [a] = case a of
  (HiValueBytes x) -> returnValue $ ord <$> B.unpack x
  _                -> throwError HiErrorInvalidArgument
evalFun HiFunEncodeUtf8 [a] = case a of
  (HiValueString x) -> returnValue $ encodeUtf8 x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunDecodeUtf8 [a] = case a of
  (HiValueBytes x) -> return $ fromRight HiValueNull $ HiValueString <$> decodeUtf8' x
  _                -> throwError HiErrorInvalidArgument
evalFun HiFunZip [a] = case a of
  (HiValueBytes x) -> returnValue $ compress x
  _                -> throwError HiErrorInvalidArgument
  where
    compress :: ByteString -> ByteString
    compress = toStrict . Z.compressWith
      Z.defaultCompressParams { Z.compressLevel = Z.bestCompression } . fromStrict
evalFun HiFunUnzip [a] = case a of
  (HiValueBytes x) -> return $ fromRight HiValueNull $ HiValueBytes <$> decompress x
  _                -> throwError HiErrorInvalidArgument
  where
    decompress :: ByteString -> Either Z.P.DecompressionError ByteString
    decompress = (toStrict <$>) . Z.P.decompress . fromStrict
evalFun HiFunSerialise [a] = returnValue $ serialise a
  where
    serialise :: Serialise a => a -> ByteString
    serialise = toStrict . C.serialise
evalFun HiFunDeserialise [a] = case a of
  (HiValueBytes x) -> return $ fromRight HiValueNull $ deserialise x
  _                -> throwError HiErrorInvalidArgument
  where
    deserialise :: Serialise a => ByteString -> Either DeserialiseFailure a
    deserialise = C.deserialiseOrFail . fromStrict
evalFun HiFunRead [a] = case a of
  (HiValueString x) -> returnValue $ HiActionRead $ T.unpack x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunWrite [a, b] = case (a, b) of
  (HiValueString x, HiValueString y) -> returnValue $ HiActionWrite (T.unpack x) (B.pack $ T.unpack y)
  (HiValueString x, HiValueBytes y)  -> returnValue $ HiActionWrite (T.unpack x) y
  _                                  -> throwError HiErrorInvalidArgument
evalFun HiFunMkDir [a] = case a of
  (HiValueString x) -> returnValue $ HiActionMkDir $ T.unpack x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunChDir [a] = case a of
  (HiValueString x) -> returnValue $ HiActionChDir $ T.unpack x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunParseTime [a] = case a of
  (HiValueString x) -> return $ maybe HiValueNull HiValueTime (readMaybe $ T.unpack x)
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunRand [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> rand x y
  _                                  -> throwError HiErrorInvalidArgument
  where
    rand :: MonadError HiError m => Rational -> Rational -> m HiValue
    rand x y = if int x && int y then returnValue $ HiActionRand (fromEnum x) (fromEnum y)
               else throwError HiErrorInvalidArgument
evalFun HiFunEcho [a] = case a of
  (HiValueString x) -> returnValue $ HiActionEcho x
  _                 -> throwError HiErrorInvalidArgument
evalFun HiFunCount [a] = case a of
  (HiValueString x) -> count $ T.unpack x
  (HiValueBytes x)  -> count $ ord <$> B.unpack x
  (HiValueList x)   -> count $ toList x
  _                 -> throwError HiErrorInvalidArgument
  where
    count :: (MonadError HiError m, HasHiValue a, Ord a) => [a] -> m HiValue
    count xs = returnValue $ M.fromListWith (+) ((, 1 :: Int) <$> xs)
evalFun HiFunKeys [a] = case a of
  (HiValueDict x) -> returnValue $ M.keys x
  _               -> throwError HiErrorInvalidArgument
evalFun HiFunValues [a] = case a of
  (HiValueDict x) -> returnValue $ M.elems x
  _               -> throwError HiErrorInvalidArgument
evalFun HiFunInvert [a] = case a of
  (HiValueDict x) -> returnValue $ M.fromListWith (++) ((\(k, v) -> (v, [k])) <$> M.toList x)
  _               -> throwError HiErrorInvalidArgument
evalFun _ _ = throwError HiErrorArityMismatch

-- | Permorms either indexing or slicing based on the number of arguments.
indexOrSlice :: (HiMonad m, HasHiValue l, HasHiValue a, ListLike l a) => l -> [HiValue] -> ExceptT HiError m HiValue
indexOrSlice l [a] = case a of
  (HiValueNumber x) | int x -> return $ index (fromEnum x)
  _                         -> throwError HiErrorInvalidArgument
  where
    index :: Int -> HiValue
    index n = if between 0 (L.length l - 1) n then toValue (l `L.index` n)
              else HiValueNull
indexOrSlice l [a, b] = case (a, b) of
  (HiValueNumber x, HiValueNumber y) -> toValue <$> slice l x y
  (HiValueNumber x, HiValueNull)     -> toValue <$> slice l x (toRational $ L.length l)
  (HiValueNull, HiValueNumber y)     -> toValue <$> slice l 0 y
  _                                  -> throwError HiErrorInvalidArgument
  where
    slice :: (MonadError HiError m, ListLike l a) => l -> Rational -> Rational -> m l
    slice xs x y = let
        from = adjustIdx (fromEnum x) len
        to = adjustIdx (fromEnum y) len
        len = L.length xs
      in
        if int x && between 0 len from && int y && between 0 len to
          then if from <= to then return $ (L.take (to - from) . L.drop from) xs
          else return L.empty
        else throwError HiErrorInvalidArgument

    adjustIdx :: Int -> Int -> Int
    adjustIdx x len
      | x < 0     = if x + len < 0 then 0 else x + len
      | x >= len  = len
      | otherwise = x
indexOrSlice _ _ = throwError HiErrorArityMismatch

-- | Looks up the given key in the dictionary.
dictLookup :: Monad m => Map HiValue HiValue -> [HiValue] -> ExceptT HiError m HiValue
dictLookup dict [key] = return $ fromMaybe HiValueNull (M.lookup key dict)
dictLookup _ _        = throwError HiErrorInvalidArgument
