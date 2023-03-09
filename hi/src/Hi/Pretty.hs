{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hi.Pretty
  ( prettyValue
  , prettyError
  , prettyErrorViaShow
  ) where

import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Map (Map, assocs)
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Sequence (Seq)
import Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), annotate, comma, hsep, parens, punctuate,
                                  slash, tupled, viaShow, (<+>))
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (Red), bold, color)
import Data.Time (UTCTime)
import GHC.Real (Ratio ((:%)))
import Text.Printf (printf)

import Hi.Base (HiAction (..), HiFun (..), HiValuable (toValue), HiValue (..))

import qualified Data.ByteString.Char8 as B (null, unpack)


-- | Pretty-prints a number.
--   Simply prints the number if it is an integer, calls 'prettyFraction' otherwise.
prettyNumber :: Rational -> Doc ann
prettyNumber (n :% 1) = viaShow n
prettyNumber x        = prettyFraction x

-- | Pretty-prints a fraction.
--   Calls 'prettyProper' if this is a proper fraction, and 'prettyMixed' otherwise.
prettyFraction :: Rational -> Doc ann
prettyFraction x@(n :% d) = case m of
    Nothing -> viaShow s
    Just _  -> (if abs n < d then prettyProper else prettyMixed) n d
  where (s, m) = fromRationalRepetendUnlimited x

-- | Pretty-prints a proper fraction.
prettyProper :: Integer -> Integer -> Doc ann
prettyProper num den = viaShow num <> slash <> viaShow den

-- | Pretty-prints a mixed fraction.
prettyMixed :: Integer -> Integer -> Doc ann
prettyMixed num den = viaShow q <+> (if r > 0 then "+" else "-") <+> prettyProper (abs r) den
  where (q, r) = num `quotRem` den

csv :: (a -> Doc ann) -> [a] -> Doc ann
csv f xs = hsep $ punctuate comma (f <$> xs)

-- | Pretty-prints a list.
prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList xs | null xs = "[ ]"
prettyList xs = "[" <+> csv prettyValue (toList xs) <+> "]"

-- | Pretty-prints a bytes object.
prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bs | B.null bs = "[# #]"
prettyBytes bs = "[#" <+> hsep (prettyByte <$> B.unpack bs) <+> "#]"
  where prettyByte b = pretty (printf "%02x" $ ord b :: String)

-- | Pretty-prints a dictionary.
prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict d | null d = "{ }"
prettyDict d = "{" <+> csv prettyKeyValue (assocs d) <+> "}"
  where prettyKeyValue (k, v) = prettyValue k <> ":" <+> prettyValue v

-- | Pretty-prints an action.
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionRead path        -> apply HiFunRead [toValue path]
  HiActionWrite path bytes -> apply HiFunWrite [toValue path, toValue bytes]
  HiActionMkDir dir        -> apply HiFunMkDir [toValue dir]
  HiActionChDir dir        -> apply HiFunChDir [toValue dir]
  HiActionRand from to     -> apply HiFunRand [toValue from, toValue to]
  HiActionEcho str         -> apply HiFunEcho [toValue str]
  HiActionCwd              -> "cwd"
  HiActionNow              -> "now"

  where apply f xs = viaShow f <> tupled (prettyValue <$> xs)

-- | Pretty-prints a time object.
prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime t = viaShow HiFunParseTime <> parens (viaShow $ show t)

-- | Returns the style for the given 'HiValue'.
style :: HiValue -> AnsiStyle
style HiValueNull     = bold
style (HiValueBool _) = bold
style _               = mempty

-- | Pretty-prints a 'HiValue'.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue x = annotate (style x) $ case x of
  HiValueNull       -> "null"
  HiValueNumber num -> prettyNumber num
  HiValueBool b     -> if b then "true" else "false"
  HiValueString str -> viaShow str
  HiValueFunction f -> viaShow f
  HiValueList xs    -> prettyList xs
  HiValueBytes bs   -> prettyBytes bs
  HiValueDict d     -> prettyDict d
  HiValueAction a   -> prettyAction a
  HiValueTime t     -> prettyTime t

-- | Pretty-prints the given string like an error.
prettyError :: String -> Doc AnsiStyle
prettyError = annotate (color Red <> bold) . ("error:" <+>) . pretty

-- | Pretty-prints the given object like an error (using 'Show').
prettyErrorViaShow :: Show a => a -> Doc AnsiStyle
prettyErrorViaShow = prettyError . show
