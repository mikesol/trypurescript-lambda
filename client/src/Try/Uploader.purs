module Try.Uploader where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as AXRF
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (throw)
import Halogen as H

makeUploadLink :: forall input m. Bind m => MonadAff m => EncodeJson input => input -> m String
makeUploadLink code = do
  _uploadLink' <-
    H.liftAff
      $ AX.request
          ( AX.defaultRequest
              { headers = []
              , method = Left POST
              , url = "https://5bdowlj5w5.execute-api.us-east-1.amazonaws.com/default/wags-s3-uploader"
              , content =
                ( Just
                    ( RequestBody.json
                        $ encodeJson
                            { code }
                    )
                )
              , responseFormat = AXRF.string
              }
          )
  either
    ( \_ -> do
        H.liftEffect $ throw "Could not upload content."
    )
    (pure <<< _.body)
    _uploadLink'