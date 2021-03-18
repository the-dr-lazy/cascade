let Cascade = ../package.dhall

let dependencies = [ "base-noprelude", "cascade-prelude", "chronos", "relude" ]

let cascade-core =
      { source-dirs = "src"
      , ghc-options = [ "-fplugin=Polysemy.Plugin" ]
      , dependencies =
        [ "beam-core"
        , "beam-postgres"
        , "email-validate"
        , "generic-lens"
        , "lens"
        , "polysemy"
        , "polysemy-plugin"
        , "postgresql-simple"
        , "scrypt"
        ]
      }

in    Cascade.package
    ⫽ { name = "cascade-core"
      , synopsis = "Cascade core business logic"
      , description = "Cascade core business logic"
      , dependencies
      , library = cascade-core
      }
