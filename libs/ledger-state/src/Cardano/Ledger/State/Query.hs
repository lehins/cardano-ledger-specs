{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Query where

import Control.Monad
import Control.Foldl (Fold(..))
import Control.Iterate.SetAlgebra
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.State.Schema
import Cardano.Ledger.State.Transform
import Cardano.Ledger.State.Orphans
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import Conduit
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Database.Persist.Sqlite
import Data.Text as T
import qualified Cardano.Ledger.Keys as Keys
import qualified Data.Compact.KeyMap as KeyMap
import qualified Cardano.Ledger.Credential as Credential

-- Populate database

insertGetKey ::
     ( MonadIO m
     , PersistUniqueWrite backend
     , PersistRecordBackend record backend
     , AtLeastOneUniqueKey record
     )
  => record
  -> ReaderT backend m (Key record)
insertGetKey = fmap (either entityKey id) . insertBy

insertUTxOState ::
  MonadIO m =>
  Shelley.UTxOState CurrentEra ->
  ReaderT SqlBackend m (Key UtxoState)
insertUTxOState Shelley.UTxOState {..} = do
  insert $
    UtxoState
      { utxoStateDeposited = _deposited,
        utxoStateFees = _fees,
        utxoStatePpups = _ppups
      }

insertUTxO ::
  MonadIO m =>
  Shelley.UTxO CurrentEra ->
  Key UtxoState ->
  ReaderT SqlBackend m ()
insertUTxO utxo stateKey = do
  mapM_ insertTxOut $ Map.toList (Shelley.unUTxO utxo)
  where
    insertTxOut (TxIn.TxIn txId txIx, out) = do
      txKey <-
        insert $ Tx {txInIx = fromIntegral txIx, txInId = txId, txOut = out}
      txsKey <-
        case toTxOut' mempty out of
          TxOutNoStake' out' -> do
            insert $
              Txs
                { txsInIx = fromIntegral txIx
                , txsInId = txId
                , txsOut = out'
                , txsStakeCredential = Nothing
                }
          TxOut' out' cred -> do
            credId <- insertGetKey (Credential (Keys.asWitness cred))
            insert $
              Txs
                { txsInIx = fromIntegral txIx
                , txsInId = txId
                , txsOut = out'
                , txsStakeCredential = Just credId
                }
      insert_ $
        UtxoEntry
          { utxoEntryTxs = txsKey
          , utxoEntryTx = txKey
          , utxoEntryState = stateKey
          }

insertDState :: MonadIO m => Shelley.DState C -> ReaderT SqlBackend m DStateId
insertDState Shelley.DState {..} = do
  let irDeltaReserves = Shelley.deltaReserves _irwd
  let irDeltaTreasury = Shelley.deltaTreasury _irwd
  dstateId <- insert $ DState (Enc _fGenDelegs) _genDelegs irDeltaReserves irDeltaTreasury
  forM_ (Map.toList _rewards) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (Reward dstateId credId c)
  forM_ (Map.toList _delegations) $ \(cred, spKeyHash) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness spKeyHash))
    insert_ (Delegation dstateId credId keyHashId)
  forM_ (Map.toList (biMapToMap _ptrs)) $ \(ptr, cred) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (Ptr dstateId credId ptr)
  forM_ (Map.toList (Shelley.iRReserves _irwd)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRReserves dstateId credId c)
  forM_ (Map.toList (Shelley.iRTreasury _irwd)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRTreasury dstateId credId c)
  pure dstateId

insertLedgerState ::
  MonadIO m => Shelley.LedgerState CurrentEra -> ReaderT SqlBackend m ()
insertLedgerState Shelley.LedgerState {..} = do
  stateKey <- insertUTxOState _utxoState
  insertUTxO (Shelley._utxo _utxoState) stateKey
  dstateKey <- insertDState $ Shelley._dstate _delegationState
  insert_ $
    LedgerState
      { ledgerStateUtxo = stateKey,
        ledgerStateDstate = dstateKey,
        ledgerStatePstateBin = Shelley._pstate _delegationState
      }

-- Query database

sourceUTxO ::
     MonadResource m
  => ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceUTxO =
  selectSource [] []
    .| mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))

sourceUTxOs ::
     MonadResource m
  => Map.Map (Credential.StakeCredential C) a
  -> ConduitM () (TxIn.TxIn C, TxOut') (ReaderT SqlBackend m) ()
sourceUTxOs stakeCredentials =
  selectSource [] [] .|
  mapMC
    (\(Entity _ Txs {..}) -> do
       let txi = TxIn.TxIn txsInId (fromIntegral txsInIx)
       case txsStakeCredential of
         Nothing -> pure (txi, TxOutNoStake' txsOut)
         Just credId -> do
           Credential credential <- getJust credId
           let !sharedCredential = intern (Keys.coerceKeyRole credential) stakeCredentials
           pure (txi, TxOut' txsOut sharedCredential))

foldDbUTxO ::
     MonadUnliftIO m
  => (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
  -> a -- ^ Empty acc
  -> Text -- ^ Path to Sqlite db
  -> m a
foldDbUTxO f m fp = runSqlite fp (runConduit (sourceUTxO .| foldlC f m))


-- sourceUTxOr ::
--      MonadResource m
--   => Int64 -> Int64 -> ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
-- sourceUTxOr b t =
--   selectSource [TxId >. TxKey (SqlBackendKey b) , TxId <. TxKey (SqlBackendKey t)] [] .|
--   mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))

-- foldDbUTxOr ::
--      MonadUnliftIO m
--   => Int64
--   -> Int64
--   -> (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
--   -> a -- ^ Empty acc
--   -> Text -- ^ Path to Sqlite db
--   -> m a
-- foldDbUTxOr b t f m fp = runSqlite fp (runConduit (sourceUTxOr b t .| foldlC f m))

lsid :: Key LedgerState
lsid = LedgerStateKey (SqlBackendKey 1)

getLedgerState ::
     MonadIO m
  => Shelley.UTxO CurrentEra
  -> LedgerState
  -> Shelley.DState C
  -> ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerState utxo LedgerState {..} dstate = do
  UtxoState {..} <- getJust ledgerStateUtxo
  pure
    Shelley.LedgerState
      { Shelley._utxoState =
          Shelley.UTxOState
            { Shelley._utxo = utxo
            , Shelley._deposited = utxoStateDeposited
            , Shelley._fees = utxoStateFees
            , Shelley._ppups = utxoStatePpups
            }
      , Shelley._delegationState =
          Shelley.DPState
            { Shelley._dstate = dstate
            , Shelley._pstate = ledgerStatePstateBin
            }
      }

getDStateNoSharing ::
     MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState C)
getDStateNoSharing dstateId = do
  DState {..} <- getJust dstateId
  rewards <-
    Map.fromList <$> do
      rws <- selectList [RewardDstate ==. dstateId] []
      forM rws $ \(Entity _ Reward {..}) -> do
        Credential credential <- getJust rewardCredential
        pure (Keys.coerceKeyRole credential, rewardCoin)
  delegations <-
    Map.fromList <$> do
      ds <- selectList [DelegationDstate ==. dstateId] []
      forM ds $ \(Entity _ Delegation {..}) -> do
        Credential credential <- getJust delegationCredential
        KeyHash keyHash <- getJust delegationStakePool
        pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole keyHash)
  ptrs <-
    biMapFromList const <$> do
      ps <- selectList [PtrDstate ==. dstateId] []
      forM ps $ \(Entity _ Ptr {..}) -> do
        Credential credential <- getJust ptrCredential
        pure (ptrPtr, Keys.coerceKeyRole credential)
  iRReserves <-
    Map.fromList <$> do
      ds <- selectList [IRReservesDstate ==. dstateId] []
      forM ds $ \(Entity _ IRReserves {..}) -> do
        Credential credential <- getJust iRReservesCredential
        pure (Keys.coerceKeyRole credential, iRReservesCoin)
  iRTreasury <-
    Map.fromList <$> do
      ds <- selectList [IRTreasuryDstate ==. dstateId] []
      forM ds $ \(Entity _ IRTreasury {..}) -> do
        Credential credential <- getJust iRTreasuryCredential
        pure (Keys.coerceKeyRole credential, iRTreasuryCoin)
  pure
    Shelley.DState
      { _rewards = rewards
      , _delegations = delegations
      , _ptrs = ptrs
      , _fGenDelegs = unEnc dStateFGenDelegs
      , _genDelegs = dStateGenDelegs
      , _irwd =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves
            , iRTreasury = iRTreasury
            , deltaReserves = dStateIrDeltaReserves
            , deltaTreasury = dStateIrDeltaTreasury
            }
      }

getDStateWithSharing ::
     MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState C)
getDStateWithSharing dstateId = do
  DState {..} <- getJust dstateId
  rewards <-
    Map.fromList <$> do
      rws <- selectList [RewardDstate ==. dstateId] []
      forM rws $ \(Entity _ Reward {..}) -> do
        Credential credential <- getJust rewardCredential
        pure (Keys.coerceKeyRole credential, rewardCoin)
  delegations <-
    Map.fromList <$> do
      ds <- selectList [DelegationDstate ==. dstateId] []
      forM ds $ \(Entity _ Delegation {..}) -> do
        Credential credential <- getJust delegationCredential
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        KeyHash keyHash <- getJust delegationStakePool
        pure (cred, Keys.coerceKeyRole keyHash)
  ptrs <-
    biMapFromList const <$> do
      ps <- selectList [PtrDstate ==. dstateId] []
      forM ps $ \(Entity _ Ptr {..}) -> do
        Credential credential <- getJust ptrCredential
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (ptrPtr, cred)
  iRReserves <-
    Map.fromList <$> do
      ds <- selectList [IRReservesDstate ==. dstateId] []
      forM ds $ \(Entity _ IRReserves {..}) -> do
        Credential credential <- getJust iRReservesCredential
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (cred, iRReservesCoin)
  iRTreasury <-
    Map.fromList <$> do
      ds <- selectList [IRTreasuryDstate ==. dstateId] []
      forM ds $ \(Entity _ IRTreasury {..}) -> do
        Credential credential <- getJust iRTreasuryCredential
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (cred, iRTreasuryCoin)
  pure
    Shelley.DState
      { _rewards = rewards
      , _delegations = delegations
      , _ptrs = ptrs
      , _fGenDelegs = unEnc dStateFGenDelegs
      , _genDelegs = dStateGenDelegs
      , _irwd =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves
            , iRTreasury = iRTreasury
            , deltaReserves = dStateIrDeltaReserves
            , deltaTreasury = dStateIrDeltaTreasury
            }
      }
loadDStateNoSharing :: MonadUnliftIO m => Text -> m (Shelley.DState C)
loadDStateNoSharing fp =
  runSqlite fp $ getDStateNoSharing (DStateKey (SqlBackendKey 1))

loadUTxONoSharing ::
     MonadUnliftIO m => Text -> m (Shelley.UTxO CurrentEra)
loadUTxONoSharing fp =
  runSqlite fp (Shelley.UTxO <$> runConduitFold sourceUTxO noSharing)


getLedgerStateNoSharing ::
     MonadUnliftIO m => Text -> m (Shelley.LedgerState CurrentEra)
getLedgerStateNoSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateNoSharing ledgerStateDstate
    m <- runConduitFold sourceUTxO noSharing
    ls <- getLedgerState (Shelley.UTxO m) ledgerState dstate
    pure ls

getLedgerStateDStateSharing ::
     MonadUnliftIO m => Text -> m (Shelley.LedgerState CurrentEra)
getLedgerStateDStateSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    m <- runConduitFold sourceUTxO noSharing
    ls <- getLedgerState (Shelley.UTxO m) ledgerState dstate
    pure ls

getLedgerStateWithSharing ::
     MonadUnliftIO m
  => Text
  -> m (Shelley.LedgerState CurrentEra, IntMap.IntMap (Map.Map (TxIn.TxId C) TxOut'))
getLedgerStateWithSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    let stakeCredentials = Shelley._rewards dstate
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold (sourceUTxOs stakeCredentials) txIxSharing
    pure (ls, m)

getLedgerStateDStateTxOutSharing ::
     MonadUnliftIO m
  => Text
  -> m (Shelley.LedgerState CurrentEra, Map.Map (TxIn.TxIn C) TxOut')
getLedgerStateDStateTxOutSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    let stakeCredentials = Shelley._rewards dstate
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold (sourceUTxOs stakeCredentials) noSharing
    pure (ls, m)

getLedgerStateTxOutSharing ::
     MonadUnliftIO m
  => Text
  -> m (Shelley.LedgerState CurrentEra, Map.Map (TxIn.TxIn C) TxOut')
getLedgerStateTxOutSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateNoSharing ledgerStateDstate
    let stakeCredentials = Shelley._rewards dstate
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold (sourceUTxOs stakeCredentials) noSharing
    pure (ls, m)

getLedgerStateDStateTxIxSharing ::
     MonadUnliftIO m
  => Text
  -> m ( Shelley.LedgerState CurrentEra
       , IntMap.IntMap (Map.Map (TxIn.TxId C) (Alonzo.TxOut CurrentEra)))
getLedgerStateDStateTxIxSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold sourceUTxO txIxSharing
    pure (ls, m)


getLedgerStateWithSharingKeyMap ::
     MonadUnliftIO m
  => Text
  -> m (Shelley.LedgerState CurrentEra, IntMap.IntMap (KeyMap.KeyMap TxOut'))
getLedgerStateWithSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    let stakeCredentials = Shelley._rewards dstate
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold (sourceUTxOs stakeCredentials) txIxSharingKeyMap
    pure (ls, m)

getLedgerStateDStateTxIxSharingKeyMap ::
     MonadUnliftIO m
  => Text
  -> m ( Shelley.LedgerState CurrentEra
       , IntMap.IntMap (KeyMap.KeyMap (Alonzo.TxOut CurrentEra)))
getLedgerStateDStateTxIxSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    m <- runConduitFold sourceUTxO txIxSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)


getLedgerStateDStateTxIdSharingKeyMap ::
     MonadUnliftIO m
  => Text
  -> m ( Shelley.LedgerState CurrentEra
       , KeyMap.KeyMap (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
getLedgerStateDStateTxIdSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsid
    dstate <- getDStateWithSharing ledgerStateDstate
    m <- runConduitFold sourceUTxO txIdSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)


storeLedgerState ::
     MonadUnliftIO m => FilePath -> Shelley.LedgerState CurrentEra -> m ()
storeLedgerState fp ls =
  runSqlite (T.pack fp) $ do
    runMigration migrateAll
    insertLedgerState ls

loadDbUTxO :: UTxOFold a -> FilePath -> IO a
loadDbUTxO (Fold f e g) fp =
  runSqlite (T.pack fp) (g <$> runConduit (sourceUTxO .| foldlC f e))
