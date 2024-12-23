{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "protovoice-annotation-tool"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "debug"
  , "dom-filereader"
  , "dom-indexed"
  -- , "downloadjs"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "pitches"
  , "prelude"
  , "psci-support"
  , "rationals"
  , "simple-json"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  , "protovoice-model"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
