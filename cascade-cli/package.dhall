let Cascade = ../package.dhall

let dependencies = [ "base-noprelude", "cascade-prelude" ]

let cascade =
      { source-dirs = "app"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "cascade-cli" ]
      }

let cascade-cli =
      { source-dirs = "src"
      , dependencies =
        [ "cascade-api"
        , "co-log"
        , "postgresql-simple"
        , "resource-pool"
        , "optparse-applicative"
        , "gitrev"
        , "generic-data"
        , "attoparsec"
        , "validation-selective"
        , "lens"
        , "generic-lens"
        , "network"
        , "generic-monoid"
        ]
      , generated-other-modules = [ "Paths_cascade_cli" ]
      }

let cascade-cli-test =
      { source-dirs = "test"
      , main = "Spec.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      }

in    Cascade.package
    ⫽ { name = "cascade-cli"
      , synopsis = "Cascade command line tool"
      , description = "Cascade command line tool"
      , category = "CLI"
      , dependencies
      , executables.cascade = cascade
      , library = cascade-cli
      , tests.cascade-cli-test = cascade-cli-test
      }
