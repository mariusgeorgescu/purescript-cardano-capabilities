-- | Default implementations for MonadCardanoQuery
module Cardano.Capabilities.Query.Default
  ( fetchPoolInfoDefault
  ) where

import Prelude

import Affjax (Error(..))
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Cardano.Capabilities.Query.Env (HasQueryEnv)
import Cardano.Capabilities.Query.Types (PoolInfo)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (ForeignError(..), unsafeFromForeign)

-- | Default implementation for fetching pool information
-- | Makes HTTP request to the pool info endpoint
fetchPoolInfoDefault ::
  forall m r.
  MonadAff m =>
  MonadAsk (HasQueryEnv r) m =>
  String -> m (Either String PoolInfo)
fetchPoolInfoDefault poolId = do
  env <- ask
  let
    url = env.poolInfoURL poolId
  let
    req =
      { url: url
      , method: Left GET
      , responseFormat: AXRF.json
      , headers: []
      , content: Nothing
      , password: Nothing
      , username: Nothing
      , timeout: Just $ Milliseconds 10_000_000.0
      , withCredentials: true
      }
  result <- liftAff $ AXW.request req
  case result of
    Right success -> do
      case decodeJson success.body of
        Right poolInfo -> pure $ Right poolInfo
        Left decodeErr -> pure $ Left $ "Failed to decode pool info: " <> show decodeErr
    Left (ResponseBodyError (ForeignError _msg) resp) -> do
      pure $ Left $ unsafeFromForeign resp.body
    Left e -> do
      pure $ Left $ AX.printError e

