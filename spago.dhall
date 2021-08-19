{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "protovoice-annotation-tool"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "dom-filereader"
  , "dom-indexed"
  , "downloadjs"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "partial"
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
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
