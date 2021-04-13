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
import           Cascade.Core.Data.Model.User        ( EmailAddress
                                                     , Username
                                                     )
import qualified Cascade.Core.Effect.Repository     as Repository
import qualified Cascade.Core.Effect.Repository.User
                                                    as Repository.User
import           Polysemy                            ( Member
                                                     , Sem
                                                     )

canTakeUsername :: Member Repository.UserL r => Username 'Phase.Unknown -> Sem r (Maybe (Username 'Phase.New))
canTakeUsername = fmap leftToMaybe . Repository.User.doesExistsByUsername

canTakeEmailAddress :: Member Repository.UserL r => EmailAddress 'Phase.Unknown -> Sem r (Maybe (EmailAddress 'Phase.New))
canTakeEmailAddress = fmap leftToMaybe . Repository.User.doesExistsByEmailAddress
