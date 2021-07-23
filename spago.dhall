{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "protovoice-annotation-tool"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "downloadjs"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
  , "maybe"
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
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
