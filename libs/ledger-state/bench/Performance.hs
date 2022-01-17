{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Ledger.Era
--import Cardano.Ledger.Shelley.API.Wallet

import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.Address
import Cardano.Ledger.CompactAddress
import Criterion.Main
import Data.Map.Strict as Map
import Data.Set as Set
import Options.Applicative as O

_optsParser :: Parser FilePath
_optsParser =
  option
    str
    ( long "new-epoch-state-cbor"
        <> help "EpochState needed for benchmarking operations on ledger state"
    )

main :: IO ()
main = do
  let fp = "/home/lehins/iohk/chain/mainnet/newish-ledger-state.bin"
  -- fp <-
  --   execParser $
  --     info
  --       ( optsParser
  --           <* abortOption
  --             (ShowHelpText Nothing)
  --             (long "help" <> short 'h' <> help "Display this message.")
  --       )
  --       ( header
  --           "ledger-state:performance - Benchmark for ledger state performance"
  --       )
  --runMode (Run defaultConfig) Prefix []
  -- defaultMain
  --   [ env (readNewEpochState fp <* putStrLn ("Read NewEpochState: " ++ show fp)) $ \newEpochState ->
  --       let utxo = getUTxO newEpochState
  --           (_, minTxOut) = Map.findMin $ unUTxO utxo
  --           (_, maxTxOut) = Map.findMax $ unUTxO utxo
  --           setAddr =
  --             Set.fromList [getTxOutAddr minTxOut, getTxOutAddr maxTxOut]
  --        in bgroup "getFilteredUTxO" $
  --             [ env (pure setAddr) $
  --                 bench "MinMaxTxId" . nf (getFilteredUTxO newEpochState)
  --             ]
  --   ]
  es <- readEpochState fp
  putStrLn ("Read NewEpochState: " ++ show fp)
  defaultMain
    [ env (pure es) $ \epochState ->
        let utxo = getUTxO epochState
            (_, minTxOut) = Map.findMin $ unUTxO utxo
            (_, maxTxOut) = Map.findMax $ unUTxO utxo
            setAddr =
              Set.fromList [getTxOutAddr minTxOut, getTxOutAddr maxTxOut]
         in bgroup "MinMaxTxId" $
              [ env (pure setAddr) $
                  bench "getFilteredNewUTxO" . nf (getFilteredNewUTxO utxo),
                env (pure setAddr) $
                  bench "getFilteredOldUTxO" . nf (getFilteredOldUTxO utxo)
              ]
    ]

getUTxO :: EpochState era -> UTxO era
getUTxO = _utxo . _utxoState . esLState

getFilteredNewUTxO ::
  Era era =>
  UTxO era ->
  Set (Addr (Crypto era)) ->
  UTxO era
getFilteredNewUTxO (UTxO fullUTxO) addrSet =
  UTxO $ Map.filter checkAddr fullUTxO
  where
    compactAddrSet = Set.map compactAddr addrSet
    checkAddr out =
      case getTxOutEitherAddr out of
        Left addr -> addr `Set.member` addrSet
        Right cAddr -> cAddr `Set.member` compactAddrSet

getFilteredOldUTxO ::
  Era era =>
  UTxO era ->
  Set (Addr (Crypto era)) ->
  UTxO era
getFilteredOldUTxO (UTxO fullUTxO) addrs =
  UTxO $
    Map.filter (\out -> getTxOutCompactAddr out `Set.member` addrSBSs) fullUTxO
  where
    addrSBSs = Set.map compactAddr addrs
