let Cascade = ../package.dhall

let dependencies =
      [ "aeson"
      , "base-noprelude"
      , "bytestring"
      , "cascade-prelude"
      , "containers"
      , "generic-lens"
      , "lens"
      , "polysemy"
      , "polysemy-plugin"
      , "postgresql-simple"
      , "servant"
      , "text"
      , "uuid"
      , "word8"
      ]

let cascade-api =
      { source-dirs = "src"
      , ghc-options = [ "-fplugin=Polysemy.Plugin" ]
      , dependencies =
        [ "attoparsec"
        , "beam-core"
        , "beam-postgres"
        , "chronos"
        , "cookie"
        , "either"
        , "email-validate"
        , "generic-monoid"
        , "http-media"
        , "http-types"
        , "libjwt-typed"
        , "relude"
        , "scrypt"
        , "selective"
        , "servant-server"
        , "validation-selective"
        , "wai"
        , "warp"
        ]
      }

let cascade-api-test =
      { source-dirs = "test"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies =
        [ "cascade-api"
        , "directory"
        , "filepath"
        , "free"
        , "hedgehog"
        , "http-client"
        , "http-types"
        , "lifted-async"
        , "lifted-base"
        , "managed"
        , "monad-control"
        , "port-utils"
        , "postgres-options"
        , "relude"
        , "resource-pool"
        , "servant-client"
        , "servant-client-core"
        , "tasty"
        , "tasty-hedgehog"
        , "tmp-postgres"
        , "typed-process"
        ]
      }

let cascade-api-benchmark =
      { source-dirs = "benchmark"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "gauge", "cascade-api" ]
      }

in    Cascade.package
    ⫽ { name = "cascade-api"
      , synopsis = "Cascade Web API"
      , description = "Cascade Web API"
      , category = "Web"
      , dependencies
      , library = cascade-api
      , tests.cascade-api-test = cascade-api-test
      , benchmarks.cascade-api-benchmark = cascade-api-benchmark
      }
