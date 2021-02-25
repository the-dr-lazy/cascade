{-|
Module      : Cascade.Api.Test.Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Test.Prelude (concreted) where

import           Cascade.Api.Test.Prelude.Orphans    ( )
import           Control.Lens                        ( Optic'
                                                     , Profunctor
                                                     , to
                                                     )
import           Hedgehog                            ( Concrete
                                                     , Var
                                                     , concrete
                                                     )

concreted :: (Profunctor p, Contravariant f) => Optic' p f (Var a Concrete) a
concreted = to concrete
