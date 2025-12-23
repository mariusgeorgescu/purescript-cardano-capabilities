-- | Capability type class for Cardano transaction interactions
module Cardano.Capabilities.Transaction.MonadInteraction
  ( class MonadInteraction
  , buildTransaction
  , submitTransaction
  , signTransaction
  ) where

import Prelude

import Cardano.Capabilities.Transaction.Env (HasTransactionEnv)
import Cardano.Wallet.Cip30 (Api)
import Cardano.Capabilities.Wallet.MonadCIP30 (class MonadCIP30)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff)

-- | Capability for building, signing, and submitting Cardano transactions
-- |
-- | Type parameter `a` represents the action type (e.g., DelegationAction)
-- | Type parameter `m` represents the monad
class
  ( Monad m
  , MonadAff m
  , DecodeJson a
  , EncodeJson a
  , DecodeJsonField a
  , MonadCIP30 m
  ) <= MonadInteraction a m where
  -- | Build an unsigned transaction from an action
  -- | Returns the transaction CBOR as a hex string
  buildTransaction :: forall r. HasTransactionEnv r -> Api -> a -> m (Either String String)
  
  -- | Submit a signed transaction
  -- | Takes the unsigned transaction CBOR and the witness set
  -- | Returns the transaction hash
  submitTransaction :: forall r. HasTransactionEnv r -> String -> String -> m (Either String String)
  
  -- | Sign an unsigned transaction
  -- | Returns the witness set as a hex string
  signTransaction :: Api -> String -> m (Either String String)

