-- | Query environment configuration
module Cardano.Capabilities.Query.Env
  ( QueryEnv(..)
  , _QueryEnv
  , HasQueryEnv
  ) where

import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)

-- | Environment configuration for query operations
newtype QueryEnv = QueryEnv
  { poolInfoURL :: String -> String
  }

derive instance newtypeQueryEnv :: Newtype QueryEnv _

_QueryEnv :: Iso' QueryEnv { poolInfoURL :: String -> String }
_QueryEnv = _Newtype

-- | Type alias for environments that contain query configuration
type HasQueryEnv r = { poolInfoURL :: String -> String | r }

