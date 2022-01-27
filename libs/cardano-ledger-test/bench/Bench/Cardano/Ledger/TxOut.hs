{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench.Cardano.Ledger.TxOut (benchTxOut) where

import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Val
import Control.DeepSeq
import Criterion.Main
import Data.Map.Strict (singleton)
import Data.Maybe

type A = AlonzoEra StandardCrypto

benchTxOut :: Benchmark
benchTxOut =
  let ada :: Value StandardCrypto
      ada = inject (Coin 100)
      key :: Credential 'Payment StandardCrypto
      key = KeyHashObj payAddr28
      stake :: StakeReference StandardCrypto
      stake = StakeRefBase (KeyHashObj stakeAddr28)
      addr :: Addr StandardCrypto
      addr = Addr Mainnet key stake
      value :: Value StandardCrypto
      value = Value 200 (singleton (PolicyID policyId28) (singleton assName 217))
      txOutAddr :: TxOut A
      txOutAddr = TxOut addr value SNothing
      txOutAddrAdaOnly :: TxOut A
      txOutAddrAdaOnly = TxOut addr ada SNothing
      txOutAddrAdaOnlyDataHash :: TxOut A
      txOutAddrAdaOnlyDataHash = TxOut addr ada (SJust dataHash32)
   in bgroup
        "TxOut (Alonzo)"
        [ txOutAlonzoBench "txOutAddr" txOutAddr,
          txOutAlonzoBench "txOutAddrAdaOnly" txOutAddrAdaOnly,
          txOutAlonzoBench "txOutAddrAdaonlyDataHash" txOutAddrAdaOnlyDataHash
        ]

txOutAlonzoBench :: String -> TxOut A -> Benchmark
txOutAlonzoBench name txOut' =
  env (pure txOut') $ \txOut ->
    bgroup
      name
      [ bench "TxOut" $
          nf
            (\(TxOut addr vl dh) -> addr `deepseq` vl `deepseq` dh)
            txOut,
        bench "TxOutCompact" $
          nf
            ( \case
                TxOutCompact addr vl -> addr `deepseq` vl
                TxOutCompactDH addr vl dh -> addr `deepseq` dh `deepseq` vl
            )
            txOut
      ]

payAddr28 :: KeyHash 'Payment StandardCrypto
payAddr28 =
  KeyHash $
    fromJust $
      hashFromTextAsHex "0102030405060708090a0b0c0d0e0f12131415161718191a1b1c1d1e"

stakeAddr28 :: KeyHash 'Staking StandardCrypto
stakeAddr28 =
  KeyHash $
    fromJust $
      hashFromTextAsHex "2122232425262728292a2b2c2d2e2f32333435363738393a3b3c3d3e"

dataHash32 :: DataHash StandardCrypto
dataHash32 =
  unsafeMakeSafeHash $
    fromJust $
      hashFromTextAsHex "404144434445464748494a4b4c4d4e4f505152555455565758595a5b5c5d5e5f"

policyId28 :: ScriptHash StandardCrypto
policyId28 =
  ScriptHash $
    fromJust $
      hashFromTextAsHex "6166636465666768696a6b6c6d6e6f72777475767778797a7b7c7d7e"

assName :: AssetName
assName = AssetName "Booyah"
