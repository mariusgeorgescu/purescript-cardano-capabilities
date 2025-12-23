# purescript-cardano-capabilities

A PureScript library providing capability type classes for Cardano blockchain interactions.

## Overview

This library provides:

- **MonadCIP30** - Capability for CIP-30 wallet interactions (connect, sign, submit)
- **MonadInteraction** - Capability for building, signing, and submitting Cardano transactions
- **MonadCardanoQuery** - Capability for querying blockchain data (e.g., pool information)

These capabilities are designed to be framework-agnostic and can be used with Halogen, React, or any other PureScript UI framework.

## Installation

Add to your `spago.yaml`:

```yaml
package:
  dependencies:
    - cardano-capabilities

workspace:
  extraPackages:
    cardano-capabilities:
      git: https://github.com/mariusgeorgescu/purescript-cardano-capabilities.git
      ref: main  # or specific version tag
```

## Usage

### Import the library

```purescript
import Cardano.Capabilities
```

Or import specific modules:

```purescript
import Cardano.Capabilities.Wallet.MonadCIP30 (class MonadCIP30, enableWallet)
import Cardano.Capabilities.Transaction.MonadInteraction (class MonadInteraction, buildTransaction)
import Cardano.Capabilities.Query.MonadCardanoQuery (class MonadCardanoQuery, fetchPoolInfo)
```

### Define instances for your application monad

```purescript
import Cardano.Capabilities

-- Your application environment
type Env =
  { transactionEnv :: TransactionEnv
  , queryEnv :: QueryEnv
  }

-- Provide instances for your AppM
instance monadCip30AppM :: MonadCIP30 AppM where
  enable w exts = H.liftAff $ Cip30.enable w exts
  getNetworkId = H.liftAff <<< Cip30.getNetworkId
  -- ... other methods

instance monadCardanoQueryAppM :: MonadCardanoQuery AppM where
  fetchPoolInfo = fetchPoolInfoDefault

instance monadInteractionAppM :: MonadInteraction DelegationAction AppM where
  buildTransaction = buildTransactionDefault
  submitTransaction = submitTransactionDefault
  signTransaction = signTransactionDefault
```

### Use in components

```purescript
handleAction :: forall m. MonadInteraction DelegationAction m => MonadCIP30 m => Action -> m Unit
handleAction = case _ of
  ConnectWallet walletName -> do
    api <- enableWallet walletName
    networkName <- getNetworkName api
    -- ...
    
  SubmitDelegation poolId -> do
    result <- buildTransaction env api (PoolDelegation { poolId })
    case result of
      Right txCbor -> do
        signResult <- signTransaction api txCbor
        -- ...
      Left err -> -- handle error
```

## Modules

### Wallet Capabilities (CIP-30)
- `Cardano.Capabilities.Wallet.MonadCIP30` - CIP-30 wallet capability class and utilities

### Transaction Capabilities
- `Cardano.Capabilities.Transaction.Types` - Transaction-related types (`UserAddresses`, `Interaction`, `AddWitAndSubmitParams`)
- `Cardano.Capabilities.Transaction.Env` - Transaction environment configuration (`TransactionEnv`)
- `Cardano.Capabilities.Transaction.MonadInteraction` - Transaction capability class
- `Cardano.Capabilities.Transaction.Default` - Default implementations

### Query Capabilities
- `Cardano.Capabilities.Query.Types` - Query-related types (`PoolInfo`, `PoolData`, `PoolMetaJson`)
- `Cardano.Capabilities.Query.Env` - Query environment configuration (`QueryEnv`)
- `Cardano.Capabilities.Query.MonadCardanoQuery` - Query capability class
- `Cardano.Capabilities.Query.Default` - Default implementations

### Shared
- `Cardano.Capabilities.Types` - Shared utility types and functions
- `Cardano.Capabilities` - Main re-export module

## MonadCIP30 Utility Functions

The `MonadCIP30` module includes several utility functions:

```purescript
-- Enable a wallet with CIP-30 extension
enableWallet :: forall m. MonadCIP30 m => WalletName -> m Api

-- Get user addresses
getUserAddresses :: forall m. MonadCIP30 m => Api -> m (Array String)

-- Get first address in bech32 format
getUserFirstAddressBech32 :: forall m. MonadCIP30 m => Api -> m String

-- Get human-readable network name
getNetworkName :: forall m. MonadCIP30 m => Api -> m String

-- Get ADA balance
getNativeCoinBalance :: forall m. MonadCIP30 m => Api -> m BigNum
getNativeCoinBalanceString :: forall m. MonadCIP30 m => Api -> m String

-- Get native assets
getNativeAssetsBalance :: forall m. MonadCIP30 m => Api -> m (Maybe MultiAsset)

-- Get available wallets with icons
getTheAvailableWallets :: forall m. MonadCIP30 m => m (Array (Tuple WalletName String))
```

## License

MIT
