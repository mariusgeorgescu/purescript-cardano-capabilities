-- | Capability type class for Cardano blockchain queries
module Cardano.Capabilities.Query.MonadCardanoQuery
  ( class MonadCardanoQuery
  , fetchPoolInfo
  ) where

import Prelude

import Cardano.Capabilities.Query.Env (HasQueryEnv)
import Cardano.Capabilities.Query.Types (PoolInfo)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H

-- | Capability for querying Cardano blockchain data
class
  ( Monad m
  , MonadAff m
  ) <= MonadCardanoQuery m where
  -- | Fetch pool information by pool ID
  -- | Returns pool metadata, stake info, and performance metrics
  fetchPoolInfo :: forall r. HasQueryEnv r -> String -> m (Either String PoolInfo)

-- | Instance for HalogenM - lifts queries from the base monad
instance monadCardanoQueryHalogenM :: 
  (MonadAff m, MonadCardanoQuery m) =>
  MonadCardanoQuery (HalogenM st act slots msg m) where
  fetchPoolInfo env poolId = H.lift (fetchPoolInfo env poolId)
