{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set, fromList)
import Data.Text.Prettyprint.Doc (Doc, line)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, putDoc)
import Data.Void (Void)
import System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute), OptDescr (Option), getOpt,
                              usageInfo)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Environment (getArgs)
import Text.Megaparsec.Error (ParseErrorBundle, bundleErrors, parseErrorTextPretty)

import Hi.Action (HIO (runHIO), HiPermission (..), PermissionException)
import Hi.Base (HiExpr)
import Hi.Evaluator (eval)
import Hi.Parser (parse)
import Hi.Pretty (prettyError, prettyErrorViaShow, prettyValue)


main :: IO ()
main = getArgs >>= \args ->
  case getOpt Permute flags args of
    (ps, _, []) -> run $ fromList ps
    (_, _, _)   -> putStrLn $ usageInfo "Usage: hi [-rwt]" flags

  where
    flags :: [OptDescr HiPermission]
    flags = [ Option ['r'] [] (NoArg AllowRead)  "Allow read."
            , Option ['w'] [] (NoArg AllowWrite) "Allow write."
            , Option ['t'] [] (NoArg AllowTime)  "Allow time." ]

    run :: Set HiPermission -> IO ()
    run = runHIO $ runInputT defaultSettings repl

    repl :: InputT HIO ()
    repl = getInputLine "hi> " >>= maybe (return ()) interpret

    interpret :: String -> InputT HIO ()
    interpret ln = do
      lift $ case parse ln of
        Left bundle -> mapM_ put (prettyErrors bundle)
        Right expr  -> evalExpr expr >>= putLn
      repl
      where put = liftIO . putDoc
            putLn = put . (<> line)

    prettyErrors :: ParseErrorBundle String Void -> NonEmpty (Doc AnsiStyle)
    prettyErrors bundle = prettyError . parseErrorTextPretty <$> (bundleErrors bundle)

    evalExpr :: HiExpr -> HIO (Doc AnsiStyle)
    evalExpr expr = do
      result <- try @HIO @PermissionException $ eval expr
      return $ case result of
        Left err          -> prettyErrorViaShow err
        Right (Left err)  -> prettyErrorViaShow err
        Right (Right val) -> prettyValue val
