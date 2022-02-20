let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let overrides = {=}

let additions =
      { wags =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arraybuffer-types"
          , "behaviors"
          , "control"
          , "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "event"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "free"
          , "heterogeneous"
          , "indexed-monad"
          , "integers"
          , "js-timers"
          , "lists"
          , "math"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "psci-support"
          , "record"
          , "refs"
          , "safe-coerce"
          , "sized-vectors"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "unsafe-coerce"
          , "web-events"
          , "simple-json"
          ]
        , repo = "https://github.com/mikesol/purescript-wags.git"
        , version = "v0.6.9"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "psci-support"
          , "variant"
          ]
        , repo = "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      , free =
        { dependencies =
          [ "catenable-lists"
          , "control"
          , "distributive"
          , "either"
          , "exists"
          , "foldable-traversable"
          , "invariant"
          , "lazy"
          , "maybe"
          , "prelude"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-free.git"
        , version = "master"
        }
      , typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      , wags-lib =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arrays"
          , "behaviors"
          , "control"
          , "effect"
          , "either"
          , "event"
          , "foldable-traversable"
          , "free"
          , "halogen"
          , "heterogeneous"
          , "identity"
          , "integers"
          , "lists"
          , "math"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "psci-support"
          , "record"
          , "run"
          , "safe-coerce"
          , "sized-vectors"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "unfoldable"
          , "unsafe-coerce"
          , "wags"
          ]
        , repo =
            "https://github.com/mikesol/purescript-wags-lib.git"
        , version = "v0.0.97"
        }
      }

in  upstream // overrides // additions
