{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import Data.Text.Prettyprint.Doc (Doc, line)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, putDoc)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, runInputT)
import Text.Megaparsec.Error (errorBundlePretty)

import Hi.Action (HiPermission, PermissionException, runHIO)
import Hi.Base (HiExpr)
import Hi.Evaluator (eval)
import Hi.Parser (parse)
import Hi.Pretty (prettyError, prettyValue)


main :: IO ()
main = runInputT defaultSettings repl
  where
    repl :: InputT IO ()
    repl = getInputLine "hi> " >>= maybe (return ()) interpret

    interpret :: String -> InputT IO ()
    interpret ln = do
      case parse ln of
        Left err   -> outputStr (errorBundlePretty err)
        Right expr -> liftIO $ evalExpr expr >>= putDocLn
      repl

    evalExpr :: HiExpr -> IO (Doc AnsiStyle)
    evalExpr expr = do
      result <- try @PermissionException $ runHIO (eval expr) permissions
      return $ case result of
        Left err          -> prettyError err
        Right (Left err)  -> prettyError err
        Right (Right val) -> prettyValue val

    permissions :: Set HiPermission
    permissions = [minBound ..]

    putDocLn :: Doc AnsiStyle -> IO ()
    putDocLn doc = putDoc (doc <> line)
