{-|
Module      : Cascade.Api.Effect.Time
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Time (TimeL, now, run) where


import           Polysemy                            ( Embed
                                                     , Member
                                                     , Sem
                                                     , embed
                                                     , interpret
                                                     , makeSem
                                                     )
import           Chronos                             ( Time )
import qualified Chronos

data TimeL (m :: Type -> Type) (a :: Type) where
  Now ::TimeL m Time

makeSem ''TimeL

run :: Member (Embed IO) r => Sem (TimeL ': r) a -> Sem r a
run = interpret \case
  Now -> embed Chronos.now
