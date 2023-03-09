{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text.Prettyprint.Doc (Doc, line)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, putDoc)
import Data.Void (Void)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Megaparsec.Error (ParseErrorBundle, bundleErrors, parseErrorTextPretty)

import Hi.Action (HiPermission, PermissionException, runHIO)
import Hi.Base (HiExpr)
import Hi.Evaluator (eval)
import Hi.Parser (parse)
import Hi.Pretty (prettyError, prettyErrorViaShow, prettyValue)


main :: IO ()
main = runInputT defaultSettings repl
  where
    repl :: InputT IO ()
    repl = getInputLine "hi> " >>= maybe (return ()) interpret

    interpret :: String -> InputT IO ()
    interpret ln = do
      liftIO $ case parse ln of
        Left bundle -> mapM_ putDoc (prettyErrors bundle)
        Right expr  -> evalExpr expr >>= putDocLn
      repl

    prettyErrors :: ParseErrorBundle String Void -> NonEmpty (Doc AnsiStyle)
    prettyErrors bundle = prettyError . parseErrorTextPretty <$> (bundleErrors bundle)

    evalExpr :: HiExpr -> IO (Doc AnsiStyle)
    evalExpr expr = do
      result <- try @PermissionException $ runHIO (eval expr) permissions
      return $ case result of
        Left err          -> prettyErrorViaShow err
        Right (Left err)  -> prettyErrorViaShow err
        Right (Right val) -> prettyValue val

    permissions :: Set HiPermission
    permissions = [minBound ..]

    putDocLn :: Doc AnsiStyle -> IO ()
    putDocLn doc = putDoc (doc <> line)
