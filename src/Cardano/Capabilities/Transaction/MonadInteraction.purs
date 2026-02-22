-- | Capability type class for Cardano transaction interactions
-- |
-- | This module provides a marker type class and standalone functions.
-- | To use, just declare an empty instance for your monad:
-- |
-- | ```purescript
-- | instance MonadInteraction AppM
-- | ```
module Cardano.Capabilities.Transaction.MonadInteraction
  ( class MonadInteraction
  , buildTransaction
  , submitTransaction
  , signTransaction
  ) where

import Cardano.Capabilities.Transaction.Default (buildTransactionDefault, signTransactionDefault, submitTransactionDefault)
import Cardano.Capabilities.Transaction.Env (HasTransactionEnv)
import Cardano.Capabilities.Wallet.MonadCIP30 (class MonadCIP30)
import Cardano.Wallet.Cip30 (Api)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either)
import Halogen (HalogenM)

-- | Marker type class for transaction interaction capability.
-- |
-- | Declare an empty instance for your monad:
-- | ```purescript
-- | instance MonadInteraction AppM
-- | ```
-- | Then use the standalone functions from this module.
class MonadCIP30 m <= MonadInteraction m

-- | Lift MonadInteraction through HalogenM
instance monadInteractionHalogenM :: (MonadCIP30 m, MonadInteraction m) => MonadInteraction (HalogenM st act slots msg m)

-- Standalone functions (using default implementations)

-- | Build an unsigned transaction from an action
-- | Returns the transaction CBOR as a hex string
buildTransaction
  :: forall m a r
   . MonadInteraction m
  => EncodeJson a
  => DecodeJson a
  => DecodeJsonField a
  => HasTransactionEnv r
  -> Api
  -> a
  -> m (Either String String)
buildTransaction = buildTransactionDefault

-- | Submit a signed transaction
-- | Takes the unsigned transaction CBOR and the witness set
-- | Returns the transaction hash
submitTransaction
  :: forall m r
   . MonadInteraction m
  => HasTransactionEnv r
  -> String
  -> String
  -> m (Either String String)
submitTransaction = submitTransactionDefault

-- | Sign an unsigned transaction
-- | Returns the witness set as a hex string
signTransaction
  :: forall m
   . MonadInteraction m
  => Api
  -> String
  -> m (Either String String)
signTransaction = signTransactionDefault
