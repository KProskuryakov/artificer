{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "artificer"
, dependencies =
  [ "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "integers"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "validation"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
