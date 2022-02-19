{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main where


import Aws.Lambda
import System.Environment (lookupEnv)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Glob (glob)
import Data.Maybe (Maybe(..))
import qualified System.IO as IO
import qualified Lib
import           Control.Monad (unless, foldM, when)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runLogger')
import qualified Control.Monad.State as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Writer.Strict (runWriterT)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Bifunctor (first, second, bimap)
import qualified Data.ByteString.Lazy as BL
import           Data.Default (def)
import           Data.Function (on)
import qualified Data.IORef as IORef
import           Data.List (nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.CST.Monad as CSTM
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CodeGen.JS.Printer as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Docs.Types as Docs
import qualified Language.PureScript.Errors.JSON as P
import qualified Language.PureScript.Interactive as I
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.Make.Cache as Cache
import qualified Language.PureScript.TypeChecker.TypeSearch as TS

main :: IO ()
main = do
  lambdaMode <- lookupEnv "LAMBDA_MODE"
  case lambdaMode of
    Nothing -> do
      runLambdaHaskellRuntime
        defaultDispatcherOptions
        (pure ())
        id
        (addStandaloneLambdaHandler "handler" Lib.handler)
    Just "compile" -> do
      -- Stop mangled "Compiling ModuleName" text
      IO.hSetBuffering IO.stderr IO.LineBuffering
      (portString : hostMode : inputGlobs) <- getArgs
      inputFiles <- concat <$> traverse glob inputGlobs
      e <- runExceptT $ do
        modules <- ExceptT $ I.loadAllModules inputFiles
        (exts, env) <- ExceptT . I.runMake . I.make $ map (second CST.pureResult) modules
        namesEnv <- fmap fst . runWriterT $ foldM P.externsEnv P.primEnv exts
        pure ()
      case e of
        Left err -> print err >> exitFailure
        Right _ -> print "Compilation success"
    Just mode -> fail ("Unknown LAMBDA_MODE: " <> mode)