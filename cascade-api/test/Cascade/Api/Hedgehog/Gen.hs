{-|
Module      : Cascade.Api.Hedgehog.Gen
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen (uuid, replicateAtLeastOne) where

import           Cascade.Api.Hedgehog.Gen.Prelude
import           Data.UUID.Util                      ( UnpackedUUID(..) )
import qualified Data.UUID.Util                     as UUID
import           Hedgehog                            ( MonadGen )
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

uuid :: MonadGen m => m UUID
uuid = do
  time_low            <- Gen.word32 Range.linearBounded
  time_mid            <- Gen.word16 Range.linearBounded
  time_hi_and_version <- Gen.word16 Range.linearBounded
  clock_seq_hi_res    <- Gen.word8 Range.linearBounded
  clock_seq_low       <- Gen.word8 Range.linearBounded
  node_0              <- Gen.word8 Range.linearBounded
  node_1              <- Gen.word8 Range.linearBounded
  node_2              <- Gen.word8 Range.linearBounded
  node_3              <- Gen.word8 Range.linearBounded
  node_4              <- Gen.word8 Range.linearBounded
  node_5              <- Gen.word8 Range.linearBounded

  pure $ UUID.pack (UnpackedUUID { .. })

replicateAtLeastOne :: Enum a => Bounded a => MonadGen g => a -> Word8 -> g [a]
replicateAtLeastOne x n = do
  xs <- Gen.list (Range.singleton . fromIntegral <| n - 1) Gen.enumBounded
  Gen.shuffle <| x : xs
