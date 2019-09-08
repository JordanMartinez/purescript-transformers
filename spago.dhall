{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "transformers"
, dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "distributive"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "indexed-monad"
    , "identity"
    , "lazy"
    , "maybe"
    , "newtype"
    , "prelude"
    , "psci-support"
    , "tailrec"
    , "tuples"
    , "unfoldable"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
