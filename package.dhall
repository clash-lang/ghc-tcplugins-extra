let defs = ./defaults.dhall

in  let mods =
          [ "GhcApi.Constraint"
          , "GhcApi.Predicate"
          , "GhcApi.GhcPlugins"
          , "Internal.Type"
          , "Internal.Constraint"
          , "Internal.Evidence"
          ]

    in  let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

        in    defs
            ⫽ { name = "ghc-tcplugins-extra"
              , synopsis = "Utilities for writing GHC type-checker plugins"
              , description =
                  ''
                  Utilities for writing GHC type-checker plugins, such as
                  creating constraints, with a stable API covering multiple
                  GHC releases.''
              , category = "Type System"
              , github = "clash-lang/ghc-tcplugins-extra"
              , license = "BSD2"
              , license-file = "LICENSE"
              , flags.deverror
                =
                { description =
                    "Enables `-Werror` for development mode and TravisCI"
                , default = False
                , manual = True
                }
              , library =
                { source-dirs = "src"
                , dependencies = [ "base >=4.8 && <5" ]
                , exposed-modules = "GHC.TcPluginM.Extra"
                , other-modules = "Internal"
                , when =
                  [ { condition = "impl(ghc >= 9.2) && impl(ghc < 9.4)"
                    , source-dirs = [ "src-ghc-tree", "src-ghc-9.2" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=9.2 && <9.4"
                        , mixin = [] : List Text
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 9.0) && impl(ghc < 9.2)"
                    , source-dirs = [ "src-ghc-tree", "src-ghc-9.0" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=9.0 && <9.2"
                        , mixin = [] : List Text
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.10.0) && impl(ghc < 9.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.10" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.10 && <9.0"
                        , mixin = [] : List Text
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.8" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.8 && <8.10"
                        , mixin =
                          [ "hiding ()"
                          , "(TcRnTypes as TcRnTypes)"
                          , "(Type as Type)"
                          , "(TcRnTypes as Constraint)"
                          , "(Type as Predicate)"
                          ]
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.6" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.6 && <8.8"
                        , mixin =
                          [ "hiding ()"
                          , "(TcRnTypes as TcRnTypes)"
                          , "(Type as Type)"
                          , "(TcRnTypes as Constraint)"
                          , "(Type as Predicate)"
                          ]
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.4.0) && impl(ghc < 8.6.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.4" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.4 && <8.6"
                        , mixin =
                          [ "hiding ()"
                          , "(TcRnTypes as TcRnTypes)"
                          , "(Type as Type)"
                          , "(TcRnTypes as Constraint)"
                          , "(Type as Predicate)"
                          ]
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.2.0) && impl(ghc < 8.4.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.2" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.2 && <8.4"
                        , mixin =
                          [ "hiding ()"
                          , "(TcRnTypes as TcRnTypes)"
                          , "(Type as Type)"
                          , "(TcRnTypes as Constraint)"
                          , "(Type as Predicate)"
                          ]
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 8.0.0) && impl(ghc < 8.2.0)"
                    , source-dirs = [ "src-ghc-flat", "src-ghc-8.0" ]
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=8.0 && <8.2"
                        , mixin =
                          [ "hiding ()"
                          , "(TcRnTypes as TcRnTypes)"
                          , "(Type as Type)"
                          , "(TcRnTypes as Constraint)"
                          , "(Type as Predicate)"
                          ]
                        }
                      ]
                    , other-modules = mods
                    }
                  , { condition = "impl(ghc >= 7.10.0) && impl(ghc < 8.0.0)"
                    , source-dirs = [ "src-ghc-cpp" ] : List Text
                    , dependencies =
                      [ { name = "ghc"
                        , version = ">=7.10 && <8.0"
                        , mixin = [] : List Text
                        }
                      ]
                    , other-modules = [] : List Text
                    }
                  ]
                }
              }
