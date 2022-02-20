module Try.Url where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either(..), either)
import Data.String.Base64 (decode)
import Effect.Aff (Aff)


getRawB64Url :: String -> ExceptT String Aff String
getRawB64Url b64url = ExceptT do
  url <- pure $ either show identity $ decode b64url
  AX.get AXRF.string url >>= case _ of
    Left e ->
      pure $ Left $ "Unable to load file from url: \n" <> printError e
    Right { status } | status >= StatusCode 400 ->
      pure $ Left $ "Received error status code: " <> show status
    Right { body } ->
      pure $ Right body
