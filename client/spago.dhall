{ name = "try-purescript"
, dependencies =
  [ "ace"
  , "aff"
  , "affjax"
  , "argonaut-codecs"
  , "arrays"
  , "assert"
  , "b64"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "css"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "functors"
  , "globals"
  , "halogen"
  , "halogen-css"
  , "identity"
  , "integers"
  , "js-timers"
  , "math"
  , "maybe"
  , "node-fs"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "random"
  , "refs"
  , "semirings"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}