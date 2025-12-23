# purescript-cardano-capabilities

A PureScript library providing capability type classes for Cardano blockchain interactions.

## Overview

This library provides:

- **MonadInteraction** - Capability for building, signing, and submitting Cardano transactions
- **MonadCardanoQuery** - Capability for querying blockchain data (e.g., pool information)

These capabilities are designed to be framework-agnostic and can be used with Halogen, React, or any other PureScript UI framework.

## Installation

Add to your `spago.yaml`:

```yaml
extraPackages:
  cardano-capabilities:
    git: https://github.com/mariusgeorgescu/purescript-cardano-capabilities.git
    ref: main  # or specific version tag
```

Then add `cardano-capabilities` to your dependencies.

## Usage

### Define your application monad

```purescript
import Cardano.Capabilities (class MonadInteraction, class MonadCardanoQuery)
import Cardano.Capabilities.Transaction.Env (TransactionEnv(..))
import Cardano.Capabilities.Query.Env (QueryEnv(..))

-- Your application environment
type Env =
  { transactionEnv :: TransactionEnv
  , queryEnv :: QueryEnv
  , -- other app-specific fields
  }

-- Provide instances for your AppM
instance monadCardanoQueryAppM :: MonadCardanoQuery AppM where
  fetchPoolInfo = fetchPoolInfoDefault

instance monadInteractionAppM :: MonadInteraction DelegationAction AppM where
  buildTransaction = buildTransactionDefault
  submitTransaction = submitTransactionDefault
  signTransaction = signTransactionDefault
```

### Use in components

```purescript
handleAction :: forall m. MonadInteraction DelegationAction m => Action -> m Unit
handleAction = case _ of
  SubmitDelegation poolId -> do
    result <- buildTransaction env api (PoolDelegation { poolId })
    case result of
      Right txCbor -> do
        signResult <- signTransaction api txCbor
        -- ...
      Left err -> -- handle error
```

## Modules

- `Cardano.Capabilities` - Main re-export module
- `Cardano.Capabilities.Types` - Shared utility types
- `Cardano.Capabilities.Transaction.Types` - Transaction-related types
- `Cardano.Capabilities.Transaction.Env` - Transaction environment configuration
- `Cardano.Capabilities.Transaction.MonadInteraction` - Transaction capability class
- `Cardano.Capabilities.Transaction.Default` - Default implementations
- `Cardano.Capabilities.Query.Types` - Query-related types (PoolInfo, etc.)
- `Cardano.Capabilities.Query.Env` - Query environment configuration
- `Cardano.Capabilities.Query.MonadCardanoQuery` - Query capability class
- `Cardano.Capabilities.Query.Default` - Default implementations

## License

MIT

