let defs = ./defaults.dhall

in  let ghc = { name = "ghc", mixin = [] : List Text }

    in  let gin =
                ghc
              ⫽ { mixin =
                  [ "hiding ()"
                  , "(TcRnTypes as TcRnTypes)"
                  , "(Type as Type)"
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
                      [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                        , source-dirs = [ "src-ghc-tree", "src-ghc-9.2" ]
                        , dependencies = [ ghc ⫽ { version = ">=9.2 && <9.4" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 9.0) && impl(ghc < 9.2)"
                        , source-dirs = [ "src-ghc-tree", "src-ghc-9.0" ]
                        , dependencies = [ ghc ⫽ { version = ">=9.0 && <9.2" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.10.0) && impl(ghc < 9.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.10" ]
                        , dependencies =
                          [ ghc ⫽ { version = ">=8.10 && <9.0" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.8" ]
                        , dependencies =
                          [ gin ⫽ { version = ">=8.8 && <8.10" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.6" ]
                        , dependencies = [ gin ⫽ { version = ">=8.6 && <8.8" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.4.0) && impl(ghc < 8.6.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.4" ]
                        , dependencies = [ gin ⫽ { version = ">=8.4 && <8.6" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.2.0) && impl(ghc < 8.4.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.2" ]
                        , dependencies = [ gin ⫽ { version = ">=8.2 && <8.4" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 8.0.0) && impl(ghc < 8.2.0)"
                        , source-dirs = [ "src-ghc-flat", "src-ghc-8.0" ]
                        , dependencies = [ gin ⫽ { version = ">=8.0 && <8.2" } ]
                        , other-modules = mods
                        }
                      , { condition = "impl(ghc >= 7.10.0) && impl(ghc < 8.0.0)"
                        , source-dirs = [ "src-ghc-cpp" ]
                        , dependencies =
                          [ ghc ⫽ { version = ">=7.10 && <8.0" } ]
                        , other-modules = [] : List Text
                        }
                      ]
                    }
                  }
