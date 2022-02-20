module Try.Config where

import Prelude

loaderUrl :: String
loaderUrl = "https://purescript-wags.netlify.app/js/output"

compileUrl :: String
compileUrl = "https://supvghemaw.eu-west-1.awsapprunner.com"

tag :: String
tag = "wags"

mainGitHubExample :: String
mainGitHubExample = "/mikesol/trypurescript/" <> tag <> "/client/examples/Main.purs"
