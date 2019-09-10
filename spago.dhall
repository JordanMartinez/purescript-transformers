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
    , "identity"
    , "indexed-monad"
    , "lazy"
    , "maybe"
    , "newtype"
    , "prelude"
    , "psci-support"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "unfoldable"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
