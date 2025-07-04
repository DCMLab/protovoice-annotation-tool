{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20250311/packages.dhall
        sha256:a5f3cbdf8fe4785c18303875b4d74bbdbcdb334332f92ee0e7db4c8af8524286

in  upstream
  with simple-json =
    { dependencies =
      [ "typelevel-prelude"
      , "record"
      , "variant"
      , "nullable"
      , "foreign-object"
      , "foreign"
      , "exceptions"
      , "arrays"
      ]
    , repo = "https://github.com/justinwoo/purescript-simple-json.git"
    , version = "v9.0.0"
    }
  with dom-filereader =
    { dependencies = [ "web-html", "aff", "arraybuffer-types", "web-file" ]
    , repo = "https://github.com/nwolverson/purescript-dom-filereader.git"
    , version = "v7.0.0"
    }
  with halogen-svg-elems =
    { dependencies =
      [ "aff"
      , "arrays"
      , "effect"
      , "foldable-traversable"
      , "halogen"
      , "halogen-hooks"
      , "maybe"
      , "newtype"
      , "prelude"
      , "safe-coerce"
      , "strings"
      , "tuples"
      , "typelevel-prelude"
      , "web-dom"
      , "web-uievents"
      ]
    , repo =
        "https://github.com/JordanMartinez/purescript-halogen-svg-elems.git"
    , version = "v5.0.3"
    }
  with pitches =
    { dependencies =
      [ "aff"
      , "console"
      , "control"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "group"
      , "lists"
      , "maybe"
      , "partial"
      , "prelude"
      , "quickcheck"
      , "simple-json"
      , "spec"
      , "spec-quickcheck"
      , "string-parsers"
      , "strings"
      ]
    , repo = "https://github.com/DCMLab/purescript-pitches.git"
    , version = "eb84662668102c6b1f84f9a4419d315e4bf25538"
    }
  with protovoice-model = ./model/spago.dhall as Location
