{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "protovoice-viewer"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "pitches"
  , "prelude"
  , "protovoice-model"
  , "psci-support"
  , "random"
  , "rationals"
  , "simple-json"
  , "transformers"
  , "tuples"
  , "web-dom"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
