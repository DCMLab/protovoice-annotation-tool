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
  , "js-maps"
  , "lists"
  , "maybe"
  , "nullable"
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
  , "web-dom"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
