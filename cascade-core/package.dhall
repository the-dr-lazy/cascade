let Cascade = ../package.dhall

let dependencies =
      [ "base-noprelude", "cascade-prelude", "chronos", "relude", "text" ]

let cascade-core =
      { source-dirs = "src"
      , other-modules = [ "Cascade.Core.Internal.Data.Text.Username" ]
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
