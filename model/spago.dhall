{ name = "protovoice-model"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "pitches"
  , "prelude"
  , "psci-support"
  , "rationals"
  , "simple-json"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
