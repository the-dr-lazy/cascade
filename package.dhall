let default-extensions =
    -- Based on
    -- https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
      [ "ApplicativeDo"
      , "Arrows"
      , "BangPatterns"
      , "BlockArguments"
      , "ConstraintKinds"
      , "DataKinds"
      , "DefaultSignatures"
      , "DeriveAnyClass"
      , "DeriveFoldable"
      , "DeriveFunctor"
      , "DeriveGeneric"
      , "DeriveLift"
      , "DeriveTraversable"
      , "DerivingStrategies"
      , "DerivingVia"
      , "DuplicateRecordFields"
      , "EmptyCase"
      , "ExistentialQuantification"
      , "FlexibleContexts"
      , "FlexibleInstances"
      , "FunctionalDependencies"
      , "GADTs"
      , "GeneralizedNewtypeDeriving"
      , "InstanceSigs"
      , "KindSignatures"
      , "LambdaCase"
      , "MultiParamTypeClasses"
      , "MultiWayIf"
      , "NamedFieldPuns"
      , "OverloadedLabels"
      , "OverloadedStrings"
      , "PatternSynonyms"
      , "PolyKinds"
      , "RankNTypes"
      , "RecordWildCards"
      , "RoleAnnotations"
      , "ScopedTypeVariables"
      , "StandaloneDeriving"
      , "StrictData"
      , "TemplateHaskell"
      , "TupleSections"
      , "TypeApplications"
      , "TypeFamilies"
      , "TypeFamilyDependencies"
      , "TypeOperators"
      ]

let ghc-options =
    -- For details on warnings:
    -- https://downloads.haskell.org/~ghc/master/users-guide/using-warnings.html
    -- This list taken from https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3#
    -- Enable all warnings with -Weverything, then disable the ones we don’t care about
      [ "-Weverything"
      , "-Wno-missing-exported-signatures"
      , "-Wno-missing-import-lists"
      , "-Wno-implicit-prelude"
      , "-Wno-missed-specialisations"
      , "-Wno-all-missed-specialisations"
      , "-Wno-unsafe"
      , "-Wno-safe"
      , "-Wno-missing-local-signatures"
      , "-Wno-monomorphism-restriction"
      ]

let package =
      { version = "0.0.0"
      , author = "Mohammad Hasani <the-dr-lazy@pm.me>"
      , copyright = "2021 Mohammad Hasani"
      , build-type = "Simple"
      , license-file = "LICENSE"
      , extra-doc-files = [ "CHANGELOG.md" ]
      , extra-source-files = [ "package.dhall" ]
      , tested-with = [ "GHC == 8.8.4" ]
      , ghc-options
      , default-extensions
      }

in  { default-extensions, ghc-options, package }
