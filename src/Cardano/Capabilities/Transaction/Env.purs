-- | Transaction environment configuration
module Cardano.Capabilities.Transaction.Env
  ( TransactionEnv(..)
  , _TransactionEnv
  , HasTransactionEnv
  ) where

import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)

-- | Environment configuration for transaction operations
newtype TransactionEnv = TransactionEnv
  { buildTxURL :: String
  , submitTxURL :: String
  , allowedNetworkId :: Int
  }

derive instance newtypeTransactionEnv :: Newtype TransactionEnv _

_TransactionEnv :: Iso' TransactionEnv { buildTxURL :: String, submitTxURL :: String, allowedNetworkId :: Int }
_TransactionEnv = _Newtype

-- | Type alias for environments that contain transaction configuration
type HasTransactionEnv r = { buildTxURL :: String, submitTxURL :: String, allowedNetworkId :: Int | r }

