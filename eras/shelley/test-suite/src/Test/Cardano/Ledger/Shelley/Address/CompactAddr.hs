{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Address.CompactAddr where

import Cardano.Ledger.Address (Addr (..), serialiseAddr)
import qualified Cardano.Ledger.CompactAddress as CA
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either
import Data.Maybe (isJust)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck

propValidateNewDecompact :: forall crypto. CC.Crypto crypto => Addr crypto -> Property
propValidateNewDecompact addr =
  let compact = SBS.toShort $ serialiseAddr addr
      decompactedOld = CA.deserializeShortAddr @crypto compact
      decompactedNew = CA.decodeAddrShort @crypto compact
   in isJust decompactedOld .&&. decompactedOld === decompactedNew

propCompactAddrRoundTrip :: CC.Crypto crypto => Addr crypto -> Bool
propCompactAddrRoundTrip addr =
  let compact = CA.compactAddr addr
      decompact = CA.decompactAddr compact
   in addr == decompact

propCompactSerializationAgree :: Addr crypto -> Bool
propCompactSerializationAgree addr =
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
   in sbs == SBS.toShort (serialiseAddr addr)

propDecompactErrors :: forall crypto. CC.Crypto crypto => Addr crypto -> Gen Property
propDecompactErrors addr = do
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
      bs = SBS.fromShort sbs
      flipHeaderBit b =
        case BS.uncons bs of
          Just (h, bsTail) -> BS.cons (complementBit h b) bsTail
          Nothing -> error "Impossible: CompactAddr can't be empty"
      mingleHeader = do
        b <- elements $ case addr of
          Addr {} -> [1, 2, 3, 7]
          AddrBootstrap {} -> [0 .. 7]
        pure ("Header", flipHeaderBit b)
      mingleAddLength = do
        NonEmpty xs <- arbitrary
        pure ("Add Length", bs <> BS.pack xs)
      mingleDropLength = do
        n <- chooseInt (1, BS.length bs)
        pure ("Drop Length", BS.take (BS.length bs - n) bs)
  (mingler, badAddr) <-
    oneof
      [ pure ("Empty", ""),
        mingleHeader,
        mingleAddLength,
        mingleDropLength
        -- minglePayment,
        -- mingleStaking
      ]
  pure $
    counterexample
      ("Mingled address with " ++ mingler ++ "was parsed: " ++ show badAddr)
      $ isLeft $ CA.decodeAddrEither @crypto badAddr
