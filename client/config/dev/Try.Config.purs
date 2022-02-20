module Try.Config where

import Prelude

loaderUrl :: String
loaderUrl = "js/output"

compileUrl :: String
compileUrl = "http://localhost:8081"

tag :: String
tag = "wags"

mainGitHubExample :: String
mainGitHubExample = "/mikesol/trypurescript/" <> tag <> "/client/examples/Main.purs"
