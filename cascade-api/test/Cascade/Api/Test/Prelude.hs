module Cascade.Api.Test.Prelude
  ( concreted
  ) where

import           Cascade.Api.Test.Prelude.Orphans   ( )
import           Control.Lens                   ( Optic'
                                                , Profunctor
                                                , to
                                                )
import           Hedgehog                       ( Concrete
                                                , Var
                                                , concrete
                                                )

concreted :: (Profunctor p, Contravariant f) => Optic' p f (Var a Concrete) a
concreted = to concrete
