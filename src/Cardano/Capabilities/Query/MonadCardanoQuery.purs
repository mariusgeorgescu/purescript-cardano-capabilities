-- | Capability type class for Cardano blockchain queries
-- |
-- | This module provides a marker type class and standalone functions.
-- | To use, just declare an empty instance for your monad:
-- |
-- | ```purescript
-- | instance MonadCardanoQuery AppM
-- | ```
module Cardano.Capabilities.Query.MonadCardanoQuery
  ( class MonadCardanoQuery
  , fetchPoolInfo
  ) where

import Cardano.Capabilities.Query.Default (fetchPoolInfoDefault)
import Cardano.Capabilities.Query.Env (HasQueryEnv)
import Cardano.Capabilities.Query.Types (PoolInfo)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)

-- | Marker type class for Cardano query capability.
-- |
-- | Declare an empty instance for your monad:
-- | ```purescript
-- | instance MonadCardanoQuery AppM
-- | ```
-- | Then use the standalone functions from this module.
class MonadAff m <= MonadCardanoQuery m

-- | Lift MonadCardanoQuery through HalogenM
instance monadCardanoQueryHalogenM :: (MonadAff m, MonadCardanoQuery m) => MonadCardanoQuery (HalogenM st act slots msg m)

-- Standalone functions (using default implementations)

-- | Fetch pool information by pool ID
-- | Returns pool metadata, stake info, and performance metrics
fetchPoolInfo
  :: forall m r
   . MonadCardanoQuery m
  => HasQueryEnv r
  -> String
  -> m (Either String PoolInfo)
fetchPoolInfo = fetchPoolInfoDefault
