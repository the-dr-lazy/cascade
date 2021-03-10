let Cascade = ../package.dhall

let dependencies = [ "base-noprelude", "cascade-prelude" ]

let cascade =
      { source-dirs = "app"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "cascade-api", "postgresql-simple", "resource-pool" ]
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
      , tests.cascade-cli-test = cascade-cli-test
      }
