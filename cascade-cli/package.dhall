let Cascade = ../package.dhall

let dependencies = [ "cascade-prelude" ]

let cascade =
      { source-dirs = "app"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "cascade-cli" ]
      }

let cascade-cli =
      { source-dirs = "src"
      , dependencies =
        [ "attoparsec"
        , "cascade-api"
        , "chronos"
        , "co-log"
        , "generic-data"
        , "generic-lens"
        , "generic-monoid"
        , "gitrev"
        , "lens"
        , "network"
        , "optparse-applicative"
        , "postgresql-simple"
        , "resource-pool"
        , "text"
        , "validation-selective"
        , "vector"
        ]
      , generated-other-modules = [ "Paths_cascade_cli" ]
      }

let cascade-cli-test =
      { source-dirs = "test"
      , main = "Spec.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      }

in      Cascade.package
    //  { name = "cascade-cli"
        , synopsis = "Cascade command line tool"
        , description = "Cascade command line tool"
        , category = "CLI"
        , dependencies
        , executables.cascade = cascade
        , library = cascade-cli
        , tests.cascade-cli-test = cascade-cli-test
        }
