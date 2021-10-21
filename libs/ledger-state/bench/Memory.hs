{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Weigh
import Control.Monad
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Query
import Data.Map.Strict as Map
import qualified Data.Text as T
import Options.Applicative as O


data Opts = Opts
  { -- | Json file to use UTxO state from.
    optsUtxoJsonFile :: Maybe FilePath,
    -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsLedgerStateBinaryFile :: Maybe FilePath,
    -- | Path to Sqlite database file.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)



optsParser :: Parser Opts
optsParser =
  Opts <$>
  option
    (Just <$> str)
    (long "utxo-json" <>
     O.value Nothing <>
     help
       "Benchmark loading Json file with UTxO into memory.") <*>
  option
    (Just <$> str)
    (long "new-epoch-state-cbor" <>
     O.value Nothing <>
     help
       ("Benchmark loading CBOR encoded NewEpochState into memory.")) <*>
  option
    (Just <$> str)
    (long "new-epoch-state-sqlite" <>
     O.value Nothing <>
     help
       ("Run various benchmarks on LedgerState representations"))

main :: IO ()
main = do
  opts <-
    execParser $
    info
      (optsParser <*
       abortOption
         (ShowHelpText Nothing)
         (long "help" <> short 'h' <> help "Display this message."))
      (header "ledger-state:memory - Tool for analyzing memory consumption of ledger state")
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs]
  mainWith $ do
    setColumns cols
    forM_ (optsUtxoJsonFile opts) $ \fp -> do
      wgroup "UTxO" $ do
        wgroup "No TxOut" $ do
          io "IntMap (KeyMap TxId ())" (loadJsonUTxO txIxSharingKeyMap_) fp
          io "KeyMap (IntMap TxId ())" (loadJsonUTxO txIdSharingKeyMap_) fp
          io "IntMap (Map TxId ())" (loadJsonUTxO txIxSharing_) fp
          io "Map TxIn ()" (loadJsonUTxO noSharing_) fp
    forM_ (optsLedgerStateBinaryFile opts) $ \binFp -> do
      io "NewEpochState" loadNewEpochState binFp
    forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
      let dbFp = T.pack dbFpStr
      -- wgroup "Baseline" $ do
      --   io "DState" loadDStateNoSharing dbFp
      --   io "UTxO" loadUTxONoSharing dbFp
      --   io "LedgerState" getLedgerStateNoSharing dbFp
      wgroup "UTxO (No TxOut)" $ do
        io "IntMap (KeyMap TxId ())" (loadDbUTxO txIxSharingKeyMap_) dbFpStr
        io "KeyMap TxId (IntMap TxId ())" (loadDbUTxO txIdSharingKeyMap_) dbFpStr
        io "IntMap (Map TxId ())" (loadDbUTxO txIxSharing_) dbFpStr
        io "Map TxIn ()" (loadDbUTxO noSharing_) dbFpStr
      -- wgroup "LedgerState" $ do
      --   wgroup "Share DState" $ do
      --       io "IntMap (KeyMap TxId TxOut)" getLedgerStateDStateTxIxSharingKeyMap dbFp
      --       io "KeyMap TxId (IntMap TxOut)" getLedgerStateDStateTxIdSharingKeyMap dbFp
            -- io "IntMap (Map TxId TxOut)" getLedgerStateDStateTxIxSharing dbFp
        --   io "Map TxIn TxOut" getLedgerStateDStateSharing dbFp
        --   wgroup "Share TxOut StakeCredential" $ do
        --     io "Map TxIn TxOut'" getLedgerStateDStateTxOutSharing dbFp
        -- wgroup "Share TxOut StakeCredential" $ do
        --   io "Map TxIn TxOut'" getLedgerStateTxOutSharing dbFp
        -- wgroup "No Sharing" $ do
            -- wgroup "Share TxOut StakeCredential" $ do
            --   io "IntMap (KeyMap TxId TxOut')" getLedgerStateWithSharingKeyMap dbFp
            --   io "IntMap (Map TxId TxOut')" getLedgerStateWithSharing dbFp
