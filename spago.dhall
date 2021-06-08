{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "protovoice-annotation-tool"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
