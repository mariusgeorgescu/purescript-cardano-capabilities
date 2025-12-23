-- | Default implementations for MonadInteraction
module Cardano.Capabilities.Transaction.Default
  ( buildTransactionDefault
  , buildTransactionFromInteraction
  , signTransactionDefault
  , submitTransactionDefault
  ) where

import Prelude

import Affjax (Error(..))
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AXW
import Cardano.Capabilities.Transaction.Env (HasTransactionEnv)
import Cardano.Capabilities.Transaction.Types (AddWitAndSubmitParams(..), Interaction(..), UserAddresses(..))
import Cardano.Wallet.Cip30 (Api)
import Cardano.Capabilities.Wallet.MonadCIP30 (class MonadCIP30)
import Cardano.Capabilities.Wallet.MonadCIP30 as Cip30
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (ForeignError(..), unsafeFromForeign)

-- | Default implementation for signing transactions
signTransactionDefault ::
  forall m.
  MonadAff m =>
  MonadCIP30 m =>
  Api -> String -> m (Either String String)
signTransactionDefault api unsignedTxCbor = do
  signedTx <- Cip30.signTx api unsignedTxCbor true
  pure $ Right signedTx

-- | Default implementation for building transactions
-- | Fetches wallet addresses and delegates to buildTransactionFromInteraction
buildTransactionDefault ::
  forall a m r.
  MonadAff m =>
  EncodeJson a =>
  DecodeJson a =>
  DecodeJsonField a =>
  MonadCIP30 m =>
  HasTransactionEnv r -> Api -> a -> m (Either String String)
buildTransactionDefault env api a = do
  usedAddresses <- Cip30.getUsedAddresses api Nothing
  changeAddress <- Cip30.getChangeAddress api
  stakeAddresses <- Cip30.getRewardAddresses api
  let interaction =
        Interaction
          { action: a
          , recipient: Nothing
          , userAddresses:
              UserAddresses
                { usedAddresses: usedAddresses
                , changeAddress: changeAddress
                , stakeAddresses: stakeAddresses
                }
          }
  buildTransactionFromInteraction env interaction

-- | Build transaction from an Interaction structure
-- | Makes HTTP request to the build endpoint
buildTransactionFromInteraction ::
  forall a m r.
  MonadAff m =>
  EncodeJson a =>
  DecodeJson a =>
  DecodeJsonField a =>
  HasTransactionEnv r -> Interaction a -> m (Either String String)
buildTransactionFromInteraction env interaction = do
  let
    req =
      { url: env.buildTxURL
      , method: Left POST
      , responseFormat: AXRF.json
      , headers: []
      , content: Just $ AXRB.Json $ encodeJson interaction
      , password: Nothing
      , username: Nothing
      , timeout: Just $ Milliseconds 10_000_000.0
      , withCredentials: true
      }
  result <- liftAff $ AXW.request req
  case result of
    Right success -> do
      case decodeJson @String success.body of
        Right txCbor -> pure $ Right txCbor
        Left decodeErr -> pure $ Left $ "Failed to decode transaction: " <> show decodeErr
    Left (ResponseBodyError (ForeignError _msg) resp) ->
      pure $ Left $ unsafeFromForeign resp.body
    Left e ->
      pure $ Left $ AX.printError e

-- | Default implementation for submitting transactions
-- | Makes HTTP request to the submit endpoint
submitTransactionDefault ::
  forall m r.
  MonadAff m =>
  HasTransactionEnv r -> String -> String -> m (Either String String)
submitTransactionDefault env unsignedTxCbor signedTx = do
  let
    req =
      { url: env.submitTxURL
      , method: Left POST
      , responseFormat: AXRF.json
      , headers: []
      , content: Just $ AXRB.Json $ encodeJson (AddWitAndSubmitParams { tx_unsigned: unsignedTxCbor, tx_wit: signedTx })
      , password: Nothing
      , username: Nothing
      , timeout: Just $ Milliseconds 10_000_000.0
      , withCredentials: true
      }
  result <- liftAff $ AXW.request req
  case result of
    Right success -> do
      case decodeJson @String success.body of
        Right txCBOR -> pure $ Right txCBOR
        Left decodeErr -> pure $ Left $ "Failed to decode response: " <> show decodeErr
    Left (ResponseBodyError (ForeignError _msg) resp) ->
      pure $ Left $ unsafeFromForeign resp.body
    Left e ->
      pure $ Left $ AX.printError e

