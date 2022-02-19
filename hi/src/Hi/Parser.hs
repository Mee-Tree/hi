module Hi.Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (label), ParseErrorBundle, Parsec, between, choice, count, eof,
                        many, manyTill, notFollowedBy, runParser, sepBy, sepBy1, sepEndBy, try,
                        (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, letterChar, space, space1, string,
                             symbolChar)

import Hi.Base (HiAction (HiActionCwd, HiActionNow), HiExpr (..), HiFun (..), HiValue (..))

import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.Text as T (pack)
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral, lexeme, scientific, signed, symbol)


type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

symbol' :: String -> Parser String
symbol' s = (lexeme . try) (string s <* notFollowedBy symbolChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses a 'HiFun'.
pHiFun :: Parser HiFun
pHiFun = label "function" $
  choice $ (\x -> x <$ symbol (show x)) <$> [minBound ..]

-- | Parses a 'HiAction' with no args (e.g. 'HiActionNow').
pHiAction :: Parser HiAction
pHiAction = label "action" $ choice
  [ HiActionCwd <$ symbol "cwd"
  , HiActionNow <$ symbol "now" ]

-- | Parses a number.
pRational :: Parser Rational
pRational = label "number" $
  toRational <$> lexeme (L.signed space L.scientific)

-- | Parses a boolean.
pBool :: Parser Bool
pBool = label "boolean" $ choice
  [ True  <$ symbol "true"
  , False <$ symbol "false" ]

-- | Parses null.
pNull :: Parser ()
pNull = void $ symbol "null"

-- | Parses a string.
pString :: Parser Text
pString = label "string" $
  T.pack <$> (char '\"' *> manyTill L.charLiteral (symbol "\""))

-- | Parses a byte (exactly two hex symbols).
pByte :: Parser Char
pByte = chr . read . ("0x" ++) <$> count 2 hexDigitChar
-- pByte = chr <$> (lookAhead hexadecimal <* count 2 hexDigitChar)

-- | Parses a bytes object.
pBytes :: Parser ByteString
pBytes = label "bytes" $
  B.pack <$> (symbol "[#" *> (pByte `sepEndBy` space1) <* symbol "#]")

-- | Parses a list.
pList :: Parser HiExpr
pList = label "list" $
  apply HiFunList <$> (symbol "[" *> (pHi `sepBy` symbol ",") <* symbol "]")

-- | Parses a key-value pair. Useful for 'pDict'.
pKeyValue :: Parser (HiExpr, HiExpr)
pKeyValue = label "key-value" $
  (,) <$> (pHi <* symbol ":") <*> pHi

-- | Parses a dictionary.
pDict :: Parser HiExpr
pDict = label "dictionary" $
  HiExprDict <$> (symbol "{" *> (pKeyValue `sepBy` symbol ",") <* symbol "}")

-- | Parses a 'HiValue'.
pHiValue :: Parser HiValue
pHiValue = choice
  [ HiValueNumber   <$> pRational
  , HiValueBool     <$> pBool
  , HiValueNull     <$  pNull
  , HiValueString   <$> pString
  , HiValueBytes    <$> pBytes
  , HiValueFunction <$> pHiFun
  , HiValueAction   <$> pHiAction ]

-- | Parses an identifier after the dot.
pIdentifier :: Parser HiExpr
pIdentifier = lexeme $ HiExprValue . HiValueString . T.pack . intercalate "-" <$> word `sepBy1` char '-'
  where word = (:) <$> letterChar <*> many alphaNumChar

-- | Parses a 'HiExpr'. Doesn't parse operators in the top layer.
pHiExpr :: Parser HiExpr
pHiExpr = do
  val <- parens pHi <|> (HiExprValue <$> pHiValue) <|> pList <|> pDict
  applies <- many $ (flip HiExprApply <$> (args <|> dot)) <|> (HiExprRun <$ symbol "!")
  return $ foldl (&) val applies

  where args = parens $ pHi `sepBy` symbol ","
        dot  = (: []) <$> (char '.' *> pIdentifier)

apply :: HiFun -> [HiExpr] -> HiExpr
apply f = HiExprApply (HiExprValue $ HiValueFunction f)

-- | Parses the whole language.
pHi :: Parser HiExpr
pHi = makeExprParser pHiExpr
  [ [ binaryL (HiFunDiv            <$ symbol' "/")
    , binaryL (HiFunMul            <$ symbol "*") ]
  , [ binaryL (HiFunAdd            <$ symbol "+")
    , binaryL (HiFunSub            <$ symbol "-") ]
  , [ binaryN (HiFunNotLessThan    <$ symbol ">=")
    , binaryN (HiFunNotGreaterThan <$ symbol "<=")
    , binaryN (HiFunLessThan       <$ symbol "<")
    , binaryN (HiFunGreaterThan    <$ symbol ">")
    , binaryN (HiFunEquals         <$ symbol "==")
    , binaryN (HiFunNotEquals      <$ symbol "/=") ]
  , [ binaryR (HiFunAnd            <$ symbol "&&") ]
  , [ binaryR (HiFunOr             <$ symbol "||") ] ]
  where
    binary :: Parser HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
    binary p = (\f x y -> f [x, y]) . apply <$> p

    binaryN, binaryL, binaryR :: Parser HiFun -> Operator Parser HiExpr
    binaryN = InfixN . binary
    binaryL = InfixL . binary
    binaryR = InfixR . binary

-- | Runs the main parser ('pHi') on the given string.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof pHi) "Hi.Parser"
