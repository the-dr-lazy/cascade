{-|
Module      : Cascade.Api.Test.Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Test.Prelude
    ( concreted
    , evalUnion
    ) where

import           Cascade.Api.Test.Prelude.Orphans ()
import           Control.Lens                     ( Optic', Profunctor, to )
import           Hedgehog                         ( Concrete, MonadTest, Var, concrete, evalMaybe )
import           Servant.API.UVerb.Union          ( IsMember, Union, matchUnion )

concreted :: (Profunctor p, Contravariant f) => Optic' p f (Var a Concrete) a
concreted = to concrete

evalUnion :: forall a as m . HasCallStack => MonadTest m => Show a => IsMember a as => Union as -> m a
evalUnion = evalMaybe . matchUnion @a
