{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Massiv where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Shelley.TxBody hiding (TxId, TxIn)
import Cardano.Ledger.State.UTxO
import Control.DeepSeq
--import Data.Compact.KVVector
import Data.Compact.KVArray
import Data.Massiv.Array as A
-- import qualified Data.Vector as VB
-- import qualified Data.Vector.Storable as VS

-- data SnapShotM crypto =
--   SnapShotM
--     { ssStake :: !(KVVector VB.Vector VS.Vector (KVPair (Credential 'Staking crypto) Coin))
--     , ssDelegations :: !(KVVector VB.Vector VB.Vector (KVPair (Credential 'Staking crypto) (KeyHash 'StakePool crypto)))
--     , ssPoolParams :: !(KVVector VB.Vector VB.Vector (KVPair (KeyHash 'StakePool crypto) (PoolParams crypto)))
--     }

data SnapShotM crypto = SnapShotM
  { ssStake :: !(KVVector B S (Credential 'Staking crypto) Coin),
    ssDelegations :: !(KVVector B B (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    ssPoolParams :: !(KVVector B B (KeyHash 'StakePool crypto) (PoolParams crypto))
  }

instance NFData (SnapShotM C) where
  rnf (SnapShotM s d p) = s `deepseq` d `deepseq` rnf p

data SnapShotsM crypto = SnapShotsM
  { ssPstakeMark :: !(SnapShotM crypto),
    ssPstakeSet :: !(SnapShotM crypto),
    ssPstakeGo :: !(SnapShotM crypto),
    ssFeeSS :: !Coin
  }

instance NFData (SnapShotsM C) where
  rnf (SnapShotsM r s g f) = r `deepseq` s `deepseq` g `deepseq` rnf f
