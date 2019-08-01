{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "higher-order"
, dependencies =
    [ "catenable-lists"
    , "const"
    , "effect"
    , "errors"
    , "generics-rep"
    , "lists"
    , "ordered-collections"
    , "orders"
    , "profunctor"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
}
