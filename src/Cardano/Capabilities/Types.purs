-- | Shared utility types and functions for Cardano capabilities
module Cardano.Capabilities.Types
  ( getDecodedJson
  ) where

import Prelude

import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Exception (throw)

-- | Extract a decoded JSON value or throw an error
getDecodedJson :: forall a. Either JsonDecodeError a -> Effect a
getDecodedJson = either (throw <<< printJsonDecodeError) pure

