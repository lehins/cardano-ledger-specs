{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Embed instances for (AlonzoEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export several sets of property tests for the Alonzo Era.
--     Also expert some functions that can be used to debug the Alonzo Era property test generators.
module Test.Cardano.Ledger.Alonzo.Trials
  ( alonzoPropertyTests,
    fastPropertyTests,
    genstuff,
    genAlonzoTx,
    genShelleyTx,
    genAlonzoBlock,
    genShelleyBlock,
    adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    minimalPropertyTests,
    onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    poolProperties,
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
    propertyTests,
    relevantCasesAreCovered,
    removedAfterPoolreap,
    go,
    go2,
    payscript,
    stakescript,
    scripts,
    dataspace,
    scriptspace,
    theutxo,
    alls,
    d1,
    d2,d3,d4,
  )
where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBBODY)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts (ppScript)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Pretty (PDoc, PrettyA(..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (Embed (..), IRC (..), STS (..))
import Data.Default.Class (Default (def))
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Shelley.Spec.Ledger.BlockChain (Block)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    DState,
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState,
    UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainPredicateFailure (..), ChainState (..))
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..), LedgerPredicateFailure (UtxowFailure))
-- import Shelley.Spec.Ledger.UTxO(UTxO(..))
import System.Timeout
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..), DataSpace(..), ScriptSpace(..), hashData)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..), genUtxo0, allScripts)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.PropertyTests
  ( adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    minimalPropertyTests,
    onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    poolProperties,
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
    propertyTests,
    relevantCasesAreCovered,
    removedAfterPoolreap,
  )
import Cardano.Ledger.Alonzo.TxBody()
import Test.Tasty
import Test.Tasty.QuickCheck

import Cardano.Ledger.Alonzo.Data(Data(..))
import qualified PlutusTx as P (Data (..))
import Cardano.Ledger.SafeHash(SafeHash, hashAnnotated)
import Cardano.Ledger.Hashes(EraIndependentData)


-- ======================================================================
-- These instances are needed to make property tests in the Alonzo era

instance Embed (AlonzoBBODY (AlonzoEra TestCrypto)) (CHAIN (AlonzoEra TestCrypto)) where
  wrapFailed = BbodyFailure

instance Embed (AlonzoUTXOW (AlonzoEra TestCrypto)) (LEDGER (AlonzoEra TestCrypto)) where
  wrapFailed = UtxowFailure

-- ======================================================================================
-- It is incredably hard to debug property test generators.  These functions mimic the
-- set up of a property test, so one can inspect some randomly generatated transactions

-- | Different property test generators depend upon a wide variety of of inputs. This function generates random
--     versions of all these inputs, and lets the user select which of these inputs he needs to make a generator.
--     See genAlonzoTx and genAlonzoBlock as examples of its use.
genstuff ::
  (EraGen era, Default (State (Core.EraRule "PPUP" era))) =>
  Proxy era ->
  ( GenEnv era ->
    ChainState era ->
    NewEpochState era ->
    EpochState era ->
    LedgerState era ->
    Core.PParams era ->
    Shelley.Spec.Ledger.LedgerState.UTxOState era ->
    DPState (Crypto era) ->
    DState (Crypto era) ->
    PState (Crypto era) ->
    Gen b
  ) ->
  Gen b
genstuff proxy f =
  do
    let genenv = (genEnv proxy)
    either' <- mkGenesisChainState genenv (IRC ())
    case either' of
      Left _z -> error ("OOPS")
      Right chainstate ->
        let newepochstate = chainNes chainstate
            epochstate = nesEs newepochstate
            ledgerstate = esLState epochstate
            pparams = esPp epochstate
            utxostate = _utxoState ledgerstate
            dpstate = _delegationState ledgerstate
            dstate = _dstate dpstate
            pstate = _pstate dpstate
         in (f genenv chainstate newepochstate epochstate ledgerstate pparams utxostate dpstate dstate pstate)

-- ======================================================================
-- The following genXXX let one observe example generated XXX things
-- these are very usefull to visualize what the the EraGen instances are doing.

ap :: Proxy (AlonzoEra TestCrypto)
ap = Proxy @(AlonzoEra TestCrypto)

-- An initial (mostly empty) LedgerEnv
ledgerEnv :: forall era. Default (Core.PParams era) => LedgerEnv era
ledgerEnv = LedgerEnv (SlotNo 0) 0 def (AccountState (Coin 0) (Coin 0))

genAlonzoTx :: Gen (Core.Tx (AlonzoEra TestCrypto))
genAlonzoTx = genstuff ap (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genAlonzoBlock :: Gen (Block (AlonzoEra TestCrypto))
genAlonzoBlock = genstuff ap (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

genShelleyTx :: Gen (Core.Tx (ShelleyEra TestCrypto))
genShelleyTx =
  genstuff
    (Proxy @(ShelleyEra TestCrypto))
    (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genShelleyBlock :: Gen (Block (ShelleyEra TestCrypto))
genShelleyBlock = genstuff (Proxy @(ShelleyEra TestCrypto)) (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

-- ==================================================================================================
-- Scripts are generated when we call genEnv. They are stored in fields inside the GenEnv structure.
-- scripts, payscript, and stakescript let one observe the 'nth' generated script. Very usefull
-- when debugging a Scriptic instance.

keys :: KeySpace (AlonzoEra TestCrypto)
_constants :: Constants
dataspace :: DataSpace (AlonzoEra TestCrypto)
scriptspace :: ScriptSpace (AlonzoEra TestCrypto)
genenv0 :: GenEnv (AlonzoEra TestCrypto)
genenv0@(GenEnv keys dataspace scriptspace _constants) = genEnv (Proxy @(AlonzoEra TestCrypto))

-- In scripts, n ranges over [0..149]
scripts :: Int -> (PDoc, PDoc)
scripts n = (\(x, y) -> (ppScript x, ppScript y)) ((ksMSigScripts keys) !! n)

-- In payscript and stakescript, n ranges over [0..29]
payscript :: Int -> (String, PDoc)
payscript n = (\(x, (y, _z)) -> (show x, ppScript y)) ((Map.toList (ksIndexedPayScripts keys)) !! n)

stakescript :: Int -> (String, PDoc)
stakescript n = (\(x, (y, _z)) -> (show x, ppScript y)) ((Map.toList (ksIndexedStakeScripts keys)) !! n)

theutxo :: IO ()
theutxo = do utx <- generate (genUtxo0 genenv0)
             putStrLn(show(prettyA utx))

alls :: [(PDoc,PDoc)]
alls =  (\(x, y) -> (ppScript x, ppScript y)) <$> (allScripts @(AlonzoEra TestCrypto) _constants)

d1 :: P.Data
d1 = P.I 4

d2 :: Data (AlonzoEra TestCrypto)
d2 = (Data (P.I 4))

d3 :: SafeHash TestCrypto EraIndependentData
d3 = hashAnnotated d2

d4 :: SafeHash TestCrypto EraIndependentData
d4 = hashData @(AlonzoEra TestCrypto) d1

-- ====================================================================================
-- A few sets of property tests we can use to run in different Scenarios.

-- | The same property tests run in all the other Eras, specialized to the Alonzo Era.
alonzoPropertyTests :: TestTree
alonzoPropertyTests =
  testGroup
    "Alonzo property tests"
    [ propertyTests @(AlonzoEra TestCrypto)
    ]

-- | A select subset of all the property tests
fastPropertyTests :: TestTree
fastPropertyTests =
  testGroup
    "Fast Alonzo Property Tests"
    [ testProperty "Chain and Ledger traces cover the relevant cases" (withMaxSuccess 50 (relevantCasesAreCovered @(AlonzoEra TestCrypto))),
      testProperty "total amount of Ada is preserved (Chain)" (withMaxSuccess 50 (adaPreservationChain @(AlonzoEra TestCrypto)))
    ]

-- ============================================================================
-- When debugging property tests failures, it is usefull to run a test
-- with a given replay value. go and go2 are templates for how to do this.

go :: Int -> IO (Maybe ())
go n =
  timeout 20000000 $
    defaultMain
      ( localOption
          -- (QuickCheckReplay (Just 117392)) -- after 206 tests
          (QuickCheckReplay (Just n))
          (testProperty "preserves ADA" $ adaPreservationChain @(AllegraEra TestCrypto))
          -- (propertyTests  @(AlonzoEra TestCrypto))
          -- (testProperty "Delegation Properties" (delegProperties @(AlonzoEra TestCrypto)))
      )

go2 :: IO ()
go2 =
  defaultMain
    ( localOption
        (QuickCheckReplay (Just 213590 ))
        (testProperty "Chain and Ledger traces cover the relevant cases" (withMaxSuccess 50 (relevantCasesAreCovered @(AlonzoEra TestCrypto))))

    )
