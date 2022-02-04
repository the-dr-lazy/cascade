let Rule =
      https://raw.githubusercontent.com/kowainik/relude/f57234d406fca584807ad8c94f9db7f00f9d547e/hlint/Rule.dhall
        sha256:d79c18bd110658d881755da9b87fff66bc5666489506792f1313a63e7e96fb08

let reludeHLintRules =
      https://raw.githubusercontent.com/kowainik/relude/f57234d406fca584807ad8c94f9db7f00f9d547e/hlint/hlint.dhall
        sha256:b98dd7c42b8adf4231acf9b33b6c9cbef28d2a5c155ccdfad7b18e050d6723d4

in    reludeHLintRules
    # [ Rule.Ignore { ignore.name = "Functor law" }
      , Rule.Ignore { ignore.name = "Redundant id" }
      , Rule.Ignore { ignore.name = "Use traverseToSnd" }
      , Rule.Ignore { ignore.name = "Use newtype instead of data" }
      , Rule.Ignore { ignore.name = "Use 'lookupEnv' from Relude" }
      ]
