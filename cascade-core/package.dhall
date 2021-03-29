let Cascade = ../package.dhall

let dependencies =
      [ "base-noprelude"
      , "cascade-prelude"
      , "chronos"
      , "relude"
      , "text"
      , "uuid"
      ]

let cascade-core =
      { source-dirs = "src"
      , exposed-modules = [] : List Text
      , ghc-options = [ "-fplugin=Polysemy.Plugin" ]
      , dependencies =
        [ "attoparsec"
        , "beam-core"
        , "beam-postgres"
        , "email-validate"
        , "generic-lens"
        , "lens"
        , "polysemy"
        , "polysemy-plugin"
        , "postgresql-simple"
        , "scrypt"
        , "selective"
        , "validation-selective"
        ]
      }

in    Cascade.package
    ⫽ { name = "cascade-core"
      , synopsis = "Cascade core business logic"
      , description = "Cascade core business logic"
      , dependencies
      , library = cascade-core
      }
