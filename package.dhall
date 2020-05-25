    let defs = ./defaults.dhall

in  let testopts = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]

in    defs
    ⫽ { name =
          "ghc-tcplugins-extra"
      , synopsis =
          "Utilities for writing GHC type-checker plugins"
      , description =
          ''
          Utilities for writing GHC type-checker plugins, such as
          creating constraints, with a stable API covering multiple
          GHC releases.''
      , category =
          "Type System"
      , github =
          "clash-lang/ghc-tcplugins-extra"
      , license =
          "BSD2"
      , license-file =
          "LICENSE"
      , flags =
          { deverror =
              { description =
                  "Enables `-Werror` for development mode and TravisCI"
              , default =
                  False
              , manual =
                  True
              }
          }
      , library =
          { source-dirs =
              "src"
          , other-extensions =
              [ "CPP", "LambdaCase", "RecordWildCards", "PatternSynonyms" ]
          , exposed-modules =
              "GHC.TcPluginM.Extra"
          , other-modules = "GhcApi"
          , dependencies =
              "base >=4.8 && <5"
          , when =
              { condition =
                  "impl(ghc > 8.4.4)"
              , `then` =
                  { dependencies = "ghc-lib", source-dirs = "src-ghc-lib" }
              , `else` =
                  { dependencies =
                      "ghc >=7.10 && <8.12"
                  , source-dirs =
                      "src-ghc"
                  }
              }
          }
      }
