module Try.QueryString
  ( getQueryParams
  , getQueryStringMaybe
  , setQueryString
  , setQueryStrings
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign.Object as Object

foreign import getQueryString :: Effect String

-- | Get all of the URL's query parameters.
foreign import getQueryParams :: Effect (Object.Object String)

-- | Try to get a key from the URL's query parameters.
getQueryStringMaybe :: String -> Effect (Maybe String)
getQueryStringMaybe key = Object.lookup key <$> getQueryParams

-- | Set the value of a query string parameter
foreign import setQueryParameters :: EffectFn1 (Object.Object String) Unit

-- | Update the specified key in the URL's query parameters.
setQueryString :: String -> String -> Effect Unit
setQueryString k v = setQueryStrings (Object.singleton k v)

setQueryStrings :: Object.Object String -> Effect Unit
setQueryStrings ss = do
  params <- getQueryParams
  runEffectFn1 setQueryParameters (Object.union ss params)
