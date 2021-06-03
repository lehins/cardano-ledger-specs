{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AlonzoEra instances for EraGen and ScriptClass
module Test.Cardano.Ledger.Alonzo.AlonzoEraGen where

import Cardano.Binary (serializeEncoding', toCBOR)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data as Alonzo (AuxiliaryData (..), Data (..), DataHash)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams, extendPP, retractPP)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded, scriptsNeededFromBody)
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import Cardano.Ledger.Alonzo.Rules.Utxow (langsUsed)
import Cardano.Ledger.Alonzo.Scripts as Alonzo
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    alwaysSucceeds,
  )
import Cardano.Ledger.Alonzo.Tx (IsValidating (..), ScriptPurpose (..), ValidatedTx (..), hashWitnessPPData, rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxWitness (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core (PParams, PParamsDelta, Script, TxOut)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (policies)
import Cardano.Ledger.Pretty (PrettyA (..))
import Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary (pattern AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Tx (Tx (Tx))
import Cardano.Ledger.Val (adaOnly, (<+>), (<×>))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq ((:|>)))
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set as Set
import Data.Word (Word64)
import Debug.Trace (trace)
import GHC.Records (HasField (..))
import Plutus.V1.Ledger.Api (defaultCekCostModelParams)
import qualified PlutusTx as P (Data (..))
import qualified PlutusTx as Plutus
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, Wdrl)
import Shelley.Spec.Ledger.UTxO (UTxO (..))
import Test.Cardano.Ledger.AllegraEraGen (genValidityInterval)
import Test.Cardano.Ledger.MaryEraGen (addTokens, genMint, maryGenesisValue, policyIndex)
import Test.QuickCheck hiding ((><))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    ScriptInfo,
    TwoPhaseInfo (..),
    findPlutus,
    genNatural,
    hashData,
  )
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (Quantifier (..), ScriptClass (..))
import Test.Shelley.Spec.Ledger.Generator.Update (genM, genShelleyPParamsDelta)
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Shelley (genPParams)

ptrace :: PrettyA t => [Char] -> t -> a -> a
ptrace x y z = trace ("\n" ++ show (prettyA y) ++ "\n" ++ show x) z

occaisionally :: Hashable a => a -> Int -> String -> String
occaisionally x n s = if mod (hash x) n == 0 then trace s s else s

isKeyHashAddr :: Addr crypto -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

-- | We are choosing new TxOut to pay fees, We want only Key locked addresss with Ada only values.
vKeyLocked :: Mock c => Core.TxOut (AlonzoEra c) -> Bool
vKeyLocked txout =
  isKeyHashAddr (getField @"address" txout)
    && adaOnly (getField @"value" txout)

-- ================================================================

-- | A cost model that sets everything as being free
freeCostModel :: CostModel
freeCostModel = CostModel $ 0 <$ fromJust defaultCekCostModelParams

-- ================================================================

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

genPlutusData :: Gen Plutus.Data
genPlutusData = resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
        oneof
          [ (Plutus.I <$> arbitrary),
            (Plutus.B <$> arbitrary),
            (Plutus.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))),
            (Plutus.Constr <$> arbitrary <*> listOf (gendata (n `div` 2))),
            (Plutus.List <$> listOf (gendata (n `div` 2)))
          ]
    gendata _ = oneof [Plutus.I <$> arbitrary, Plutus.B <$> arbitrary]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet gen =
  frequency
    [ (1, pure Set.empty),
      (2, Set.fromList <$> sequence [gen]),
      (1, Set.fromList <$> sequence [gen, gen])
    ]

genAux :: forall c. Mock c => Constants -> Gen (StrictMaybe (Alonzo.AuxiliaryData (AlonzoEra c)))
genAux constants =
  do
    maybeAux <- genEraAuxiliaryData @(MaryEra c) constants
    case maybeAux of
      SNothing -> pure SNothing
      SJust (Mary.AuxiliaryData x y) ->
        SJust
          <$> ( Alonzo.AuxiliaryData
                  <$> pure x
                  <*> pure (TimelockScript <$> y)
                  <*> genSet (Alonzo.Data <$> genPlutusData)
              )

instance CC.Crypto c => ScriptClass (AlonzoEra c) where
  -- basescript _ key = TimelockScript (basescript (Proxy @(MaryEra c)) key) -- The old style from Mary
  basescript proxy key = (someLeaf proxy key)
  isKey _ (TimelockScript x) = isKey (Proxy @(MaryEra c)) x
  isKey _ (PlutusScript _) = Nothing
  isOnePhase _ (TimelockScript _) = True
  isOnePhase _ (PlutusScript _) = False
  quantify _ (TimelockScript x) = fmap TimelockScript (quantify (Proxy @(MaryEra c)) x)
  quantify _ x = Leaf x
  unQuantify _ quant = TimelockScript $ unQuantify (Proxy @(MaryEra c)) (fmap unTime quant)

unTime :: Alonzo.Script era -> Timelock (Crypto era)
unTime (TimelockScript x) = x
unTime (PlutusScript _) = error "Plutus in Timelock"

okAsCollateral :: forall c. Mock c => UTxO (AlonzoEra c) -> TxIn c -> Bool
okAsCollateral utxo inputx =
  case Map.lookup inputx (unUTxO utxo) of
    Nothing -> False
    Just outputx -> vKeyLocked outputx

genAlonzoTxBody ::
  forall c.
  Mock c =>
  GenEnv (AlonzoEra c) ->
  UTxO (AlonzoEra c) ->
  Core.PParams (AlonzoEra c) ->
  SlotNo ->
  Set.Set (TxIn c) ->
  StrictSeq (TxOut (AlonzoEra c)) ->
  StrictSeq (DCert c) ->
  Wdrl c ->
  Coin ->
  StrictMaybe (Update (AlonzoEra c)) ->
  StrictMaybe (AuxiliaryDataHash c) ->
  Gen (TxBody (AlonzoEra c), [Core.Script (AlonzoEra c)])
genAlonzoTxBody _genenv utxo pparams currentslot input txOuts certs wdrls fee updates auxDHash = do
  _low <- genM (genSlotAfter currentslot)
  _high <- genM (genSlotAfter (currentslot + 50))
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  minted <- genMint
  let (minted2, txouts2) = case addTokens (Proxy @(AlonzoEra c)) mempty pparams minted txOuts of
        Nothing -> (mempty, txOuts)
        Just os -> (minted, os)
      scriptsFromPolicies = List.map (\p -> (Map.!) policyIndex p) (Set.toList $ policies minted)
      txouts3 = fmap addMaybeDataHashToTxOut txouts2
  validityInterval <- genValidityInterval currentslot
  return
    ( TxBody
        input
        (Set.filter (okAsCollateral utxo) input) -- Set.empty -- collateral -- TODO do something better here (use genenv ?)
        txouts3
        certs
        wdrls
        fee
        validityInterval -- (ValidityInterval SNothing SNothing) -- (ValidityInterval low high)
        updates
        -- reqSignerHashes
        Set.empty -- TODO do something better here
        minted2
        -- wppHash starts out with empty Redeemers, as Remdeemers are added it is recomputed in updateEraTxBody
        (hashWitnessPPData pparams (langsUsed @(AlonzoEra c) Map.empty) (Redeemers Map.empty))
        auxDHash
        netid,
      List.map TimelockScript scriptsFromPolicies
    )

genSlotAfter :: SlotNo -> Gen SlotNo
genSlotAfter currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

-- | Gen an Alonzo PParamsDelta, by adding to a Shelley PParamsData
genAlonzoPParamsDelta ::
  forall c.
  Constants ->
  Alonzo.PParams (AlonzoEra c) ->
  Gen (Core.PParamsDelta (AlonzoEra c))
genAlonzoPParamsDelta constants pp = do
  shelleypp <- genShelleyPParamsDelta @(MaryEra c) constants (Alonzo.retractPP (Coin 100) pp)
  ada <- genM (Coin <$> choose (1, 5))
  cost <- genM (pure (Map.singleton PlutusV1 freeCostModel)) -- TODO what is a better assumption for this?
  price <- genM (Prices <$> (Coin <$> choose (0, 2)) <*> (Coin <$> choose (0, 2)))
  mxTx <- pure SNothing -- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  -- Not too small for mxV, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Shelley Era uses hard coded 4000
  mxV <- genM (genNatural 4000 5000)
  let c = SJust 25 -- percent of fee in collateral
      mxC = SJust 100 -- max number of inputs in collateral
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

genAlonzoPParams ::
  forall c.
  Constants ->
  Gen (Core.PParams (AlonzoEra c))
genAlonzoPParams constants = do
  shelleypp <- Shelley.genPParams @(MaryEra c) constants -- This ensures that "_d" field is not 0.
  ada <- (Coin <$> choose (1, 5))
  cost <- pure (Map.singleton PlutusV1 freeCostModel) -- There are no other Languages, and there must be something for PlutusV1
  price <- pure (Prices (Coin 0) (Coin 0)) -- (Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000)))
  mxTx <- pure (ExUnits (5 * bigMem + 1) (5 * bigStep + 1)) -- (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- (ExUnits <$> (choose ((20 * bigMem + 1), (30 * bigMem + 1))) <*> choose ((20 * bigStep + 1), (30 * bigStep + 1)))
  mxV <- (genNatural 4000 10000) -- This can't be too small. Shelley uses Hard coded 4000
  let c = 25 -- percent of fee in collateral
      mxC = 100 -- max number of inputs in collateral
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

-- | Since Alonzo PParams don't have this field, we have to compute something here.
instance HasField "_minUTxOValue" (Alonzo.PParams (AlonzoEra c)) Coin where
  getField _ = Coin 4000

bigStep, bigMem :: Word64
bigStep = 999 -- 9999999990
bigMem = 500 -- 50000000

instance Mock c => EraGen (AlonzoEra c) where
  genEraAuxiliaryData = genAux
  genGenesisValue = maryGenesisValue
  genEraTwoPhaseScripts =
    [ TwoPhaseInfo (alwaysSucceeds 3) (P.I 1) ("Spend", 1, P.I 1, bigMem, bigStep),
      TwoPhaseInfo (alwaysSucceeds 3) (P.I 1) ("Spend", 1, P.I 1, bigMem, bigStep),
      TwoPhaseInfo (alwaysSucceeds 3) (P.I 1) ("Spend", 1, P.I 1, bigMem, bigStep),
      TwoPhaseInfo (alwaysSucceeds 3) (P.I 1) ("Spend", 1, P.I 1, bigMem, bigStep)
    ]
  genEraTxBody = genAlonzoTxBody
  updateEraTxBody utxo pp witnesses txb coinx txin txout = new
    where
      new =
        txb
          { inputs = (inputs txb) <> txin,
            collateral = (collateral txb) <> Set.filter (okAsCollateral utxo) txin, -- In Alonzo, extra inputs also are added to collateral
            txfee = coinx,
            outputs = (outputs txb) :|> txout,
            -- The witnesses may have changed, recompute the wpphash.
            wppHash = hashWitnessPPData pp (langsUsed @(AlonzoEra c) (getField @"txscripts" witnesses)) (getField @"txrdmrs" witnesses)
          }

  addInputs txb txin = txb {inputs = (inputs txb) <> txin}

  genEraPParamsDelta = genAlonzoPParamsDelta
  genEraPParams = genAlonzoPParams
  genEraWitnesses (utxo, txbody, scriptinfo) setWitVKey mapScriptWit = new
    where
      new =
        TxWitness
          setWitVKey
          Set.empty
          mapScriptWit
          (getDataMap scriptinfo mapScriptWit)
          (Redeemers rdmrMap)
      purposeHashPairs = scriptsNeededFromBody @(AlonzoEra c) utxo txbody
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Plutus scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 mapScriptWit of
          Nothing -> ans
          Just script ->
            if isNativeScript @(AlonzoEra c) script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 scriptinfo of -- It should be one of the known Plutus Scripts
                Nothing -> ans
                Just info -> addRedeemMap txbody info purpose ans -- Add it to the redeemer map

  unsafeApplyTx (Tx bod wit auxdata) = ValidatedTx bod wit (IsValidating True) auxdata

  updateEraTx utxo scriptinfo (Tx _ _ auxdata) newbody newwit = Tx @(AlonzoEra c) newbody newerwit auxdata
    where
      draftTx = Tx @(AlonzoEra c) newbody newwit auxdata
      purposeHashPairs = scriptsNeeded @(AlonzoEra c) utxo draftTx
      TxWitness a b c d _ = newwit
      newerwit = TxWitness a b c d (Redeemers rdmrMap)
      scriptmap = txscripts newwit
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Plutus scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 scriptmap of
          Nothing -> ans
          Just script ->
            if isNativeScript @(AlonzoEra c) script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 scriptinfo of -- It should be one of the known Plutus Scripts
                Nothing -> ans
                Just info -> addRedeemMap newbody info purpose ans -- Add it to the redeemer map

  genEraGoodTxOut = vKeyLocked

addRedeemMap ::
  forall c.
  TxBody (AlonzoEra c) ->
  TwoPhaseInfo (AlonzoEra c) ->
  ScriptPurpose c ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits) ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits)
addRedeemMap body1 (TwoPhaseInfo _ _ (_, _, dat, space, steps)) purpose ans =
  case (purpose, rdptr @(AlonzoEra c) body1 purpose) of
    (Spending _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Minting _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Rewarding _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Certifying _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    _ -> ans

getDataMap :: forall era. Era era => ScriptInfo era -> Map (ScriptHash (Crypto era)) (Core.Script era) -> Map (DataHash (Crypto era)) (Data era)
getDataMap scriptinfo scrips = Map.foldlWithKey' accum Map.empty scrips
  where
    accum ans hsh _script =
      case Map.lookup hsh scriptinfo of
        Nothing -> ans
        Just (TwoPhaseInfo _script dat _redeem) -> Map.insert (hashData @era dat) (Data dat) ans

instance Mock c => MinGenTxout (AlonzoEra c) where
  calcEraMinUTxO tout pp = (utxoEntrySize tout <×> getField @"_adaPerUTxOWord" pp)
  addValToTxOut v (TxOut a u _b) = TxOut a (v <+> u) (dataFromAddr a) -- _b
  genEraTxOut genv genVal addrs = do
    values <- (replicateM (length addrs) genVal)
    let makeTxOut (addr@(Addr _network (ScriptHashObj shash) _stakeref)) val = TxOut addr val maybedatahash
          where
            (_, maybedatahash) = findPlutus genv shash
        makeTxOut addr val = TxOut addr val SNothing
    pure (zipWith makeTxOut addrs values)

-- | If an Address is script address, we can find a potential data hash for it from
--   genEraTwoPhaseScripts, which contains all known plutus scripts in the tests set.
-- If the script has is not in that map, then its data hash is SNothing.
dataFromAddr :: forall c. Mock c => Addr c -> StrictMaybe (DataHash c)
dataFromAddr (Addr _network (ScriptHashObj shash) _stakeref) =
  case List.find (\info -> shash == hashScript @(AlonzoEra c) (getScript @(AlonzoEra c) info)) genEraTwoPhaseScripts of
    Just info -> SJust (hashData @(AlonzoEra c) (getData info))
    Nothing -> SNothing
dataFromAddr _ = SNothing

addMaybeDataHashToTxOut :: Mock c => TxOut (AlonzoEra c) -> TxOut (AlonzoEra c)
addMaybeDataHashToTxOut (TxOut addr val _) = TxOut addr val (dataFromAddr addr)

someLeaf ::
  forall era.
  Era era =>
  Proxy era ->
  KeyHash 'Witness (Crypto era) ->
  Script era
someLeaf _proxy x =
  let n = hash (serializeEncoding' (toCBOR x)) -- We don't really care about the hash, we only
      slot = SlotNo (fromIntegral (mod n 200)) -- use it to pseudo-randomly pick a slot and mode
      mode = mod n 3 -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf
   in case mode of
        0 -> TimelockScript $ (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
        _ -> TimelockScript $ RequireSignature x
