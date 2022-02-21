{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda

import Data.ByteString.Lazy.UTF8 (toString, fromString)
import System.Environment (lookupEnv)
import System.FilePath.Posix (joinPath)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Glob (glob)
import Data.Maybe (Maybe(..), catMaybes)
import qualified System.IO as IO
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
import           Data.List (nubBy, foldl')
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
import qualified Language.PureScript.Make.Monad as MMo
import qualified Language.PureScript.Make.Cache as Cache
import qualified Language.PureScript.TypeChecker.TypeSearch as TS
--

data FauxInput = FauxInput
  { body :: Text
  } deriving (Generic, Show)

data Input = Input
  { inputCode :: Text
  } deriving (Generic)

data Output = OutputSuccess
  { outputJS :: Text
  , outputWarnings :: [P.JSONError]
  } | OutputFailure
  { outputError :: Error }  deriving (Generic)

instance FromJSON Input
instance FromJSON FauxInput

instance ToJSON Output where
    -- this generates a Value
    toJSON (OutputSuccess js warnings) =
        object ["js" .= js, "warnings" .= warnings]

    -- this encodes directly to a bytestring Builder
    toJSON (OutputFailure err) =
        object ["error" .= err]

instance ToJSON FauxOutput where
    -- this generates a Value
    toJSON (FauxOutput statusCode headers body) =
        object ["statusCode" .= statusCode, "headers" .= headers, "body" .= body]

----------- copy and paste
type JS = Text

data Error
  = CompilerErrors [P.JSONError]
  | OtherError Text
  deriving Generic

instance A.ToJSON Error

toCompilerErrors :: NE.NonEmpty CST.ParserError -> Error
toCompilerErrors = CompilerErrors . toJsonErrors . CST.toMultipleErrors "<file>"

toJsonErrors :: P.MultipleErrors -> [P.JSONError]
toJsonErrors = P.toJSONErrors False P.Error

-- As of PureScript 0.14 we only need the `codegen` part of `MakeActions` to run
-- Try PureScript, because we already know all dependencies are compiled, we're
-- only building one module, we don't allow FFI declarations, and we want to
-- avoid writing to the file system as much as possible.
buildMakeActions :: IORef.IORef (Maybe JS) -> Make.MakeActions Make.Make
buildMakeActions codegenRef =
  Make.MakeActions
    getInputTimestampsAndHashes
    getOutputTimestamp
    readExterns
    codegen
    ffiCodegen
    progress
    readCacheDb
    writeCacheDb
    outputPrimDocs
  where
  getInputTimestampsAndHashes :: P.ModuleName -> Make.Make (Either Make.RebuildPolicy (M.Map FilePath (UTCTime, Make.Make Cache.ContentHash)))
  getInputTimestampsAndHashes _ = pure $ Right M.empty

  getOutputTimestamp :: P.ModuleName -> Make.Make (Maybe UTCTime)
  getOutputTimestamp _ = pure Nothing

  readExterns :: P.ModuleName -> Make.Make (FilePath, Maybe P.ExternsFile)
  readExterns _ = pure ("<file>", Nothing)

  codegen :: CF.Module CF.Ann -> Docs.Module -> P.ExternsFile -> P.SupplyT Make.Make ()
  codegen m _ _ = do
    rawJs <- J.moduleToJs m Nothing
    lift $ liftIO $ IORef.writeIORef codegenRef $ Just $ P.prettyPrintJS rawJs

  -- If we ever support FFI implementations in Try PureScript then we will need
  -- to implement this function. However, we do not plan to support this feature.
  ffiCodegen :: CF.Module CF.Ann -> Make.Make ()
  ffiCodegen _ = pure ()

  progress :: Make.ProgressMessage -> Make.Make ()
  progress _ = pure ()

  readCacheDb :: Make.Make Cache.CacheDb
  readCacheDb = pure M.empty

  writeCacheDb :: Cache.CacheDb -> Make.Make ()
  writeCacheDb _ = pure ()

  outputPrimDocs :: Make.Make ()
  outputPrimDocs = pure ()

server :: Text -> [P.ExternsFile] -> P.Env -> P.Environment -> IO (Either String Output)
server code externs initNamesEnv initEnv = do
  codegenRef <- IORef.newIORef Nothing
  let makeActions = buildMakeActions codegenRef
  let compile :: Text -> IO (Either Error ([P.JSONError], JS))
      compile input
        | T.length input > 2000000 = return $ Left $ OtherError "Please limit your input to 2000000 characters"
        | otherwise = do
          case CST.parseModuleFromFile "<file>" input of
            Left parserErrors ->
              return $ Left $ toCompilerErrors parserErrors

            Right partialResult -> case CST.resFull partialResult of
              (_, Left parserErrors) ->
                return $ Left $ toCompilerErrors parserErrors

              (parserWarnings, Right m) | P.getModuleName m == P.ModuleName "Main" -> do
                (makeResult, warnings) <- Make.runMake P.defaultOptions $ Make.rebuildModule' makeActions initNamesEnv externs m
                codegenResult <- IORef.readIORef codegenRef
                return $ case makeResult of
                  Left errors ->
                    Left $ CompilerErrors $ toJsonErrors errors
                  Right _ | Just js <- codegenResult -> do
                    let ws = warnings <> CST.toMultipleWarnings "<file>" parserWarnings
                    Right (toJsonErrors ws, js)
                  Right _ ->
                    Left $ OtherError "Failed to read the results of codegen."

              (_, Right _) ->
                return $ Left $ OtherError "The name of the main module should be Main."

  response <- compile code
  return $ case response of
    Left err ->
      Right (OutputFailure err)
    Right (warnings, comp) ->
      Right (OutputSuccess comp warnings)

lookupAllConstructors :: P.Environment -> P.SourceType -> [P.SourceType]
lookupAllConstructors env = P.everywhereOnTypesM $ \case
    P.TypeConstructor ann (P.Qualified Nothing tyCon) -> P.TypeConstructor ann <$> lookupConstructor env tyCon
    other -> pure other
  where
    lookupConstructor :: P.Environment -> P.ProperName 'P.TypeName -> [P.Qualified (P.ProperName 'P.TypeName)]
    lookupConstructor env nm =
      [ q
      | (q@(P.Qualified (Just _) thisNm), _) <- M.toList (P.types env)
      , thisNm == nm
      ]

-- | (Consistently) replace unqualified type constructors and type variables with unknowns.
--
-- Also remove the @ParensInType@ Constructor (we need to deal with type operators later at some point).
replaceTypeVariablesAndDesugar :: (Text -> Int -> P.SourceType) -> P.SourceType -> P.SourceType
replaceTypeVariablesAndDesugar f ty = State.evalState (P.everywhereOnTypesM go ty) (0, M.empty) where
  go = \case
    P.ParensInType _ ty -> pure ty
    P.TypeVar _ s -> do
      (next, m) <- State.get
      case M.lookup s m of
        Nothing -> do
          let ty = f s next
          State.put (next + 1, M.insert s ty m)
          pure ty
        Just ty -> pure ty
    other -> pure other

tryParseType :: Text -> Maybe P.SourceType
tryParseType = hush . fmap (CST.convertType "<file>") . runParser CST.parseTypeP
  where
    hush = either (const Nothing) Just

    runParser :: CST.Parser a -> Text -> Either String a
    runParser p =
      bimap (CST.prettyPrintError . NE.head) snd
        . CST.runTokenParser (p <* CSTM.token CST.TokEof)
        . CST.lexTopLevel
----------- end copy and paste

data FauxOutput = FauxOutput { fauxOutputStatusCode :: Int, fauxOutputHeaders :: M.Map String String, fauxOutputBody :: Text }

applicationJson = M.singleton "Content-Type" "application/json"
textPlain = M.singleton "Content-Type" "text/plain"

handler :: [P.ExternsFile] -> P.Env -> P.Environment -> FauxInput -> Context () -> IO (Either FauxOutput FauxOutput)
handler externs initNamesEnv initEnv ipt context = do
  let code'' = body ipt
  let code' = decode (fromString $ T.unpack code'')
  case code' of
      Just code -> do
        res <- server code externs initNamesEnv initEnv
        case res of
          Right good -> pure $ Right (FauxOutput 200 applicationJson (T.pack $ toString $ encode good))
          Left bad -> pure $ Left (FauxOutput 400 textPlain (T.pack bad))
      Nothing -> pure $ Left (FauxOutput 400 textPlain "Could not parse body")