{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "artificer"
, dependencies =
  [ "arrays"
  , "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
