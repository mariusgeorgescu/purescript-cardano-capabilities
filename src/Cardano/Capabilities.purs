-- | Cardano Capabilities Library
-- |
-- | This library provides capability type classes for interacting with Cardano blockchain,
-- | including wallet operations, transaction building, signing, submission, and querying.
-- |
-- | ## Usage
-- |
-- | Import this module to get access to all capabilities:
-- |
-- | ```purescript
-- | import Cardano.Capabilities
-- | ```
-- |
-- | Or import specific modules for finer control.
module Cardano.Capabilities
  ( -- Wallet Capability (CIP-30)
    module Cardano.Capabilities.Wallet.MonadCIP30
    -- Transaction Capability
  , module Cardano.Capabilities.Transaction.MonadInteraction
  , module Cardano.Capabilities.Transaction.Default
  , module Cardano.Capabilities.Transaction.Types
  , module Cardano.Capabilities.Transaction.Env
    -- Query Capability
  , module Cardano.Capabilities.Query.MonadCardanoQuery
  , module Cardano.Capabilities.Query.Default
  , module Cardano.Capabilities.Query.Types
  , module Cardano.Capabilities.Query.Env
    -- Shared Types
  , module Cardano.Capabilities.Types
  ) where

-- Wallet
import Cardano.Capabilities.Wallet.MonadCIP30 (class MonadCIP30, enable, getApiVersion, getAvailableWallets, getBalance, getChangeAddress, getCollateral, getExtensions, getIcon, getName, getNetworkId, getRewardAddresses, getSupportedExtensions, getUnusedAddresses, getUsedAddresses, getUtxos, isEnabled, isWalletAvailable, signData, signTx, submitTx)

-- Transaction
import Cardano.Capabilities.Transaction.Default (buildTransactionDefault, buildTransactionFromInteraction, signTransactionDefault, submitTransactionDefault)
import Cardano.Capabilities.Transaction.Env (TransactionEnv(..), _TransactionEnv, HasTransactionEnv)
import Cardano.Capabilities.Transaction.MonadInteraction (class MonadInteraction, buildTransaction, signTransaction, submitTransaction)
import Cardano.Capabilities.Transaction.Types (AddWitAndSubmitParams(..), Interaction(..), UserAddresses(..), _AddWitAndSubmitParams, _Interaction, _UserAddresses)

-- Query
import Cardano.Capabilities.Query.Default (fetchPoolInfoDefault)
import Cardano.Capabilities.Query.Env (QueryEnv(..), _QueryEnv, HasQueryEnv)
import Cardano.Capabilities.Query.MonadCardanoQuery (class MonadCardanoQuery, fetchPoolInfo)
import Cardano.Capabilities.Query.Types (PoolInfo(..), PoolData(..), PoolMetaJson(..), PoolInfoMaestroResponse(..))

-- Shared
import Cardano.Capabilities.Types (getDecodedJson)
