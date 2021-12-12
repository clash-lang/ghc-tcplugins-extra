let defs = ./defaults.dhall

let version = ./version.dhall

in  let ghc = { name = "ghc", mixin = [] : List Text }

    in  let gin =
                ghc
              ⫽ { mixin =
                  [ "hiding ()"
                  , "(TcRnTypes as Constraint)"
                  , "(Type as Predicate)"
                  ]
                }

        in  let mods =
                  [ "GhcApi.Constraint"
                  , "GhcApi.Predicate"
                  , "GhcApi.GhcPlugins"
                  , "Internal.Type"
                  , "Internal.Constraint"
                  , "Internal.Evidence"
                  ]

            in    defs
                ⫽ { library =
                    { source-dirs = "src"
                    , dependencies = [ "base >=4.8 && <5" ]
                    , exposed-modules = "GHC.TcPluginM.Extra"
                    , other-modules = "Internal"
                    , when =
                      [ version "9.2" "9.4" [ "tree", "9.2" ] ghc mods
                      , version "9.0" "9.2" [ "tree", "9.0" ] ghc mods
                      , version "8.10" "9.0" [ "flat", "8.10" ] ghc mods
                      , version "8.8" "8.10" [ "flat", "8.8" ] gin mods
                      , version "8.6" "8.8" [ "flat", "8.6" ] gin mods
                      , version "8.4" "8.6" [ "flat", "8.4" ] gin mods
                      , version "8.2" "8.4" [ "flat", "8.2" ] gin mods
                      , version "8.0" "8.2" [ "flat", "8.0" ] gin mods
                      , version "7.10" "8.0" [ "cpp" ] ghc ([] : List Text)
                      ]
                    }
                  }
