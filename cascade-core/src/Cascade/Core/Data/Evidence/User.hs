{-|
Module      : Cascade.Core.Data.Evidence.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Evidence.User where

import qualified Cascade.Core.Data.Model.Phase      as Phase
import qualified Cascade.Core.Data.Model.User       as User
import qualified Cascade.Core.Effect.Repository.User
                                                    as Repository.User
import           Polysemy                            ( Sem )

canTakeUsername :: User.Username 'Phase.Unknown -> Sem r (Maybe (User.Username 'Phase.New))
canTakeUsername = leftToMaybe . Repository.User.doesExistsByUsername

canTakeEmailAddress :: User.EmailAddress 'Phase.Unknown -> Sem r (Maybe (User.EmailAddress 'Phase.New))
canTakeEmailAddress = leftToMaybe . Repository.User.doesExistsByEmailAddress
