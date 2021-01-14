module Cascade.Hedgehog.Gen
  ( uuid
  ) where

import           Data.UUID.Util                 ( UnpackedUUID(..) )
import qualified Data.UUID.Util                as UUID
import           Hedgehog                       ( MonadGen )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

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
