-- | Capability type class for Cardano blockchain queries
module Cardano.Capabilities.Query.MonadCardanoQuery
  ( class MonadCardanoQuery
  , fetchPoolInfo
  ) where

import Prelude

import Cardano.Capabilities.Query.Env (HasQueryEnv)
import Cardano.Capabilities.Query.Types (PoolInfo)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff)

-- | Capability for querying Cardano blockchain data
class
  ( Monad m
  , MonadAff m
  ) <= MonadCardanoQuery m where
  -- | Fetch pool information by pool ID
  -- | Returns pool metadata, stake info, and performance metrics
  fetchPoolInfo :: forall r. MonadAsk (HasQueryEnv r) m => String -> m (Either String PoolInfo)

