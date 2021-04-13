let Cascade = ../package.dhall

let dependencies = [ "chronos", "bytestring", "base-noprelude" ]

let cascade-prelude =
      { source-dirs = "src"
      , dependencies =
        [ "aeson"
        , "attoparsec"
        , "base64"
        , "containers"
        , "flow"
        , "lens"
        , "relude"
        , "text"
        , "uuid"
        , "validation-selective"
        , "word8"
        ]
      }

let cascade-prelude-test =
      { source-dirs = "test"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies =
        [ "cascade-prelude"
        , "hedgehog"
        , "tasty"
        , "tasty-hedgehog"
        , "tasty-hunit"
        , "word8"
        ]
      }

in    Cascade.package
    ⫽ { name = "cascade-prelude"
      , synopsis = "Cascade custom prelude"
      , description = "Cascade custom prelude"
      , dependencies
      , library = cascade-prelude
      , tests.cascade-prelude-test = cascade-prelude-test
      }
