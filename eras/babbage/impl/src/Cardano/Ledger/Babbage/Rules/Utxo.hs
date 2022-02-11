{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage.Rules.Utxo where

-- ==========================================================================
-- Utxo Depends on Utxos, which means we need to import types and operations
-- from the Utxo and Utxos files from Babbage and earlier Eras that can be reused

import Cardano.Ledger.Alonzo.Rules.Utxo
  ( AlonzoUtxoNeeds,
    FeeNeeds,
    UtxoDelta (UtxoDelta),
    UtxoEvent (..),
    UtxoPredicateFailure (..),
    genericAlonzoUtxo,
    vKeyLocked,
  )
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail)
import Cardano.Ledger.Alonzo.Tx (minfee)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..), nullRedeemers)
import Cardano.Ledger.Babbage.Collateral (collBalance, minCollateral)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), wits)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut,
    collateralInputs',
    collateralReturn',
    totalCollateral',
    txfee',
  )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Mary.Value (Value)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (coin), adaOnly)
import Control.Monad (unless)
import Control.State.Transition.Extended
  ( Embed (..),
    Rule,
    RuleType (Transition),
    STS (..),
    (?!),
  )
import qualified Data.Compact.SplitMap as SplitMap
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- ========================================================

-- | Predicate failure for the Babbage Era
data UtxoPredicateFailure era
  = AlonzoUtxoPredicateFailure !(Alonzo.UtxoPredicateFailure era)
  | UnequalCollateralReturn !Coin !Coin

deriving instance
  ( Era era,
    Show (UtxoPredicateFailure era),
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (Core.Script era)
  ) =>
  Show (BabbageUtxoPred era)

deriving instance
  ( Era era,
    Eq (UtxoPredicateFailure era),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (Core.Script era)
  ) =>
  Eq (BabbageUtxoPred era)

-- TODO FIXME  add CBOR instances

-- ================================================================================

-- | feesOK can differ from Era to Era, as new notions of fees arise. This is the Babbage version
--   See: Figure 2: Functions related to fees and collateral, in the Babbage specification
--   In the spec feesOK is a boolean function. Because wee need to handle predicate failures
--   in the implementaion, it is coded as a TransitionRule. It will return () if it succeeds,
--   and raise an error (rather than return) if any of the required parts are False.
--   This version is generic in that it can be lifted to any PredicateFailure type that
--   embeds BabbageUtxoPred era. This makes it possibly useful in future Eras.
feesOK ::
  forall era.
  ( Era era,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    -- "collateral" to get inputs to pay the fees
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Validation (NonEmpty (UtxoPredicateFailure era)) ()
feesOK pp tx (UTxO utxo) =
  let txb = getField @"body" tx
      collateral = getField @"collateral" txb -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      -- (collateral ◁ utxo)
      utxoCollateral = collateral SplitMap.◁ utxo
      bal = collBalance balance @era (UTxO utxoCollateral)
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txb
          mapMaybeValidation fromShelleyFailure $ Shelley.validateFeeTooSmallUTxO pp tx,
          -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers . txrdmrs' . wits $ tx) $
            sequenceA_
              [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
                -- Part 4: balance ∗ 100 ≥ txfee txb ∗ (collateralPercent pp)
                -- Part 5: adaOnly balance
                -- Part 6: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
                Alonzo.validateCollateral pp txb utxoCollateral bal,
                -- Part 7: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
                validateUnequalCollateralReturn txb
              ]
        ]
-- > (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
validateUnequalCollateralReturn txb =
  case collateralReturn' txb of
    SNothing -> pure ()
    SJust _txout -> failureUnless (balance == total) $ UnequalCollateralReturn balance total
      where
        total = totalCollateral' txb

-- =============================================================
-- The STS BabbageUTXO instance

-- | The uninhabited type that marks the Babbage UTxO rule
data BabbageUTXO era

instance
  forall era.
  ( Era era,
    ValidateScript era,
    -- Concrete types assumptions for the Babbage Era
    Core.Tx era ~ ValidatedTx era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    Core.Witnesses era ~ TxWitness era,
    Core.Value era ~ Value (Crypto era),
    Core.EraRule "UTXO" era ~ BabbageUTXO era,
    -- We can call the UTXOS rule
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    -- Properties needed to Show and compare the predicate failures
    Show (Core.Value era),
    Show (Core.PParamsDelta era),
    Show (Core.Script era),
    Eq (Core.Value era),
    Eq (Core.Script era),
    -- Substructural properties
    FeeNeeds era,
    AlonzoUtxoNeeds era
  ) =>
  STS (BabbageUTXO era)
  where
  type State (BabbageUTXO era) = Shelley.UTxOState era
  type Signal (BabbageUTXO era) = ValidatedTx era
  type Environment (BabbageUTXO era) = Shelley.UtxoEnv era
  type BaseM (BabbageUTXO era) = ShelleyBase
  type PredicateFailure (BabbageUTXO era) = BabbageUtxoPred era
  type Event (BabbageUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [genericAlonzoUtxo babbageUtxoDelta]

instance
  ( Era era,
    STS (BabbageUTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (Core.EraRule "UTXOS" era) ~ Event (BabbageUTXOS era),
    BaseM (BabbageUTXOS era) ~ ShelleyBase,
    PredicateFailure (Core.EraRule "UTXOS" era) ~ PredicateFailure (BabbageUTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ PredicateFailure (BabbageUTXOS era)
  ) =>
  Embed (BabbageUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = FromAlonzoUtxoFail . UtxosFailure
  wrapEvent = UtxosEvent

-- ====================================================
--  Utxo to Babbage Era


-- | The UTxO transition rule for the Alonzo eras.
utxoTransition ::
  forall era.
  ( Era era,
    ValidateScript era,
    -- instructions for calling UTXOS from AlonzoUTXO
    Embed (Core.EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    -- We leave Core.PParams abstract
    UsesPParams era,
    Core.ChainData (Core.Value era),
    Core.ChainData (Core.TxOut era),
    Core.ChainData (Core.TxBody era),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_coinsPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxValSize" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "_maxCollateralInputs" (Core.PParams era) Natural,
    Core.Tx era ~ Alonzo.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Era.TxSeq era ~ Alonzo.TxSeq era,
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    ToCBOR (Core.Value era),
    HasField "txnetworkid" (Core.TxBody era) (StrictMaybe Network)
  ) =>
  TransitionRule (AlonzoUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup _ = u

  {-   txb := txbody tx   -}
  let txb = body tx
      inputsAndCollateral =
        Set.union
          (getField @"inputs" txb)
          (getField @"collateral" txb)

  {- ininterval slot (txvld txb) -}
  runValidationTransMaybe AlonzoUtxoPredicateFailure $
    mapValidationMaybe Alonzo.fromShelleyMAFailure $
    ShelleyMA.validateOutsideValidityIntervalUTxO slot txb

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfoWithErr

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runValidationTrans AlonzoUtxoPredicateFailure $ validateOutsideForecast ei sysSt tx

  {-   txins txb ≠ ∅   -}
  runValidationStaticTrans AlonzoUtxoPredicateFailure $
    mapValidationMaybe Alonzo.fromShelleyFailure $ Shelley.validateInputSetEmptyUTxO txb

  {-   feesOK pp tx utxo   -}
  runValidation $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- inputsAndCollateral = txins txb ∪ collateral txb -}
  {- (txins txb) ∪ (collateral txb) ⊆ dom utxo   -}
  runValidationTransMaybe fromShelleyFailure $
    Shelley.validateBadInputsUTxO utxo inputsAndCollateral

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runValidationTransMaybe fromShelleyMAFailure $
    ShelleyMA.validateValueNotConservedUTxO pp utxo stakepools txb

  {-   adaID ∉ supp mint tx   -}
  runValidationStaticTransMaybe fromShelleyMAFailure $
    ShelleyMA.validateTriesToForgeADA txb

  let outputs = txouts txb
  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject (uxoEntrySizetxout ∗ coinsPerUTxOWord p) -}
  runValidation $ validateOutputTooSmallUTxO pp outputs

  {-   ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runValidation $ validateOutputTooBigUTxO pp outputs

  {- ∀ (_ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runValidationStaticTransMaybe fromShelleyFailure $
    Shelley.validateOutputBootAddrAttrsTooBig outputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runValidationStaticTransMaybe fromShelleyFailure $ Shelley.validateWrongNetwork netId txb

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runValidationStaticTransMaybe fromShelleyFailure $ Shelley.validateWrongNetworkWithdrawal netId txb

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runValidationStatic $ validateWrongNetworkInTxBody netId txb

  {- txsize tx ≤ maxTxSize pp -}
  runValidationTransMaybe fromShelleyFailure $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runValidation $ validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runValidation $ validateTooManyCollateralInputs pp txb

  trans @(Core.EraRule "UTXOS" era) =<< coerce <$> judgmentContext
