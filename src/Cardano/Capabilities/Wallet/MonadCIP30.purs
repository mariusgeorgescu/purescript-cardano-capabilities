-- | Capability type class for CIP-30 wallet interactions
-- |
-- | This module provides a marker type class and standalone functions.
-- | To use, just declare an empty instance for your monad:
-- |
-- | ```purescript
-- | instance MonadCIP30 AppM
-- | ```
module Cardano.Capabilities.Wallet.MonadCIP30
  ( class MonadCIP30
  , enable
  , getExtensions
  , getNetworkId
  , getBalance
  , getUsedAddresses
  , getUtxos
  , getCollateral
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , signTx
  , signData
  , submitTx
  , getName
  , getIcon
  , isWalletAvailable
  , getAvailableWallets
  , isEnabled
  , getApiVersion
  , getSupportedExtensions
  , enableWallet
  , getNetworkName
  , getNativeCoinBalance
  , getNativeCoinBalanceString
  , getUserFirstAddressBech32
  , getTheAvailableWallets
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Api, Bytes, Cbor, DataSignature, Extension, NetworkId, Paginate, WalletName)
import Cardano.Wallet.Cip30 as Cip30
import Csl as Csl
import Data.Array (zip, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen (HalogenM)

-- | Marker type class for CIP-30 wallet capability.
-- | 
-- | Declare an empty instance for your monad:
-- | ```purescript
-- | instance MonadCIP30 AppM
-- | ```
-- | Then use the standalone functions from this module.
class MonadAff m <= MonadCIP30 m

-- | Lift MonadCIP30 through HalogenM
instance monadCip30HalogenM :: (MonadAff m, MonadCIP30 m) => MonadCIP30 (HalogenM st act slots msg m)

-- Standalone functions (using default CIP-30 implementations)

-- | Enable a wallet with optional extensions
enable :: forall m. MonadCIP30 m => WalletName -> Array Extension -> m Api
enable w exts = liftAff $ Cip30.enable w exts

-- | Get enabled extensions for a wallet API
getExtensions :: forall m. MonadCIP30 m => Api -> m (Array Extension)
getExtensions = liftAff <<< Cip30.getExtensions

-- | Get the network ID (0 = testnet, 1 = mainnet)
getNetworkId :: forall m. MonadCIP30 m => Api -> m NetworkId
getNetworkId = liftAff <<< Cip30.getNetworkId

-- | Get wallet balance as CBOR-encoded value
getBalance :: forall m. MonadCIP30 m => Api -> m Cbor
getBalance = liftAff <<< Cip30.getBalance

-- | Get used addresses with optional pagination
getUsedAddresses :: forall m. MonadCIP30 m => Api -> Maybe Paginate -> m (Array Cbor)
getUsedAddresses api mp = liftAff $ Cip30.getUsedAddresses api mp

-- | Get UTxOs with optional amount filter and pagination
getUtxos :: forall m. MonadCIP30 m => Api -> Maybe Cbor -> Maybe Paginate -> m (Maybe (Array Cbor))
getUtxos api ma mp = liftAff $ Cip30.getUtxos api ma mp

-- | Get collateral UTxOs for a given amount
getCollateral :: forall m. MonadCIP30 m => Api -> Cbor -> m (Maybe (Array Cbor))
getCollateral api amt = liftAff $ Cip30.getCollateral api amt

-- | Get unused addresses
getUnusedAddresses :: forall m. MonadCIP30 m => Api -> m (Array Cbor)
getUnusedAddresses = liftAff <<< Cip30.getUnusedAddresses

-- | Get change address
getChangeAddress :: forall m. MonadCIP30 m => Api -> m Cbor
getChangeAddress = liftAff <<< Cip30.getChangeAddress

-- | Get reward/stake addresses
getRewardAddresses :: forall m. MonadCIP30 m => Api -> m (Array Cbor)
getRewardAddresses = liftAff <<< Cip30.getRewardAddresses

-- | Sign a transaction (partial signing supported)
signTx :: forall m. MonadCIP30 m => Api -> Cbor -> Boolean -> m Cbor
signTx api tx isPartial = liftAff $ Cip30.signTx api tx isPartial

-- | Sign arbitrary data
signData :: forall m. MonadCIP30 m => Api -> Cbor -> Bytes -> m DataSignature
signData api addr payload = liftAff $ Cip30.signData api addr payload

-- | Submit a signed transaction
submitTx :: forall m. MonadCIP30 m => Api -> Cbor -> m String
submitTx api tx = liftAff $ Cip30.submitTx api tx

-- | Get wallet name
getName :: forall m. MonadCIP30 m => WalletName -> m String
getName = liftEffect <<< Cip30.getName

-- | Get wallet icon
getIcon :: forall m. MonadCIP30 m => WalletName -> m String
getIcon = liftEffect <<< Cip30.getIcon

-- | Check if a wallet is available
isWalletAvailable :: forall m. MonadCIP30 m => WalletName -> m Boolean
isWalletAvailable = liftEffect <<< Cip30.isWalletAvailable

-- | Get list of available wallets
getAvailableWallets :: forall m. MonadCIP30 m => m (Array WalletName)
getAvailableWallets = liftEffect Cip30.getAvailableWallets

-- | Check if a wallet is enabled
isEnabled :: forall m. MonadCIP30 m => WalletName -> m Boolean
isEnabled = liftAff <<< Cip30.isEnabled

-- | Get wallet API version
getApiVersion :: forall m. MonadCIP30 m => WalletName -> m String
getApiVersion = liftEffect <<< Cip30.getApiVersion

-- | Get supported extensions for a wallet
getSupportedExtensions :: forall m. MonadCIP30 m => WalletName -> m (Array Extension)
getSupportedExtensions = liftEffect <<< Cip30.getSupportedExtensions

-- Helper utility functions

-- | Enable a wallet with default CIP-30 extensions
enableWallet :: forall m. MonadCIP30 m => WalletName -> m Api
enableWallet wname = enable wname [ { cip: 30 } ]

-- | Get human-readable network name
getNetworkName :: forall m. MonadCIP30 m => Api -> m String
getNetworkName api = do
  id <- getNetworkId api
  pure
    $ case id of
        1 -> "Mainnet"
        0 -> "Preview"
        2 -> "Preprod"
        _ -> "Unknown"

-- | Get ADA balance as BigNum (in lovelace)
getNativeCoinBalance :: forall m. MonadCIP30 m => Api -> m Csl.BigNum
getNativeCoinBalance api = do
  walletBalanceCbor <- getBalance api
  let
    balance = fromMaybe Csl.value.zero $ Csl.value.fromHex walletBalanceCbor
  pure $ Csl.bigNum.divFloor (Csl.value.coin balance) (maybe Csl.bigNum.one identity (Csl.bigNum.fromStr "1000000"))

-- | Get ADA balance as formatted string with "₳" symbol
getNativeCoinBalanceString :: forall m. MonadCIP30 m => Api -> m String
getNativeCoinBalanceString api = do
  adaBalance <- getNativeCoinBalance api
  pure $ (Csl.bigNum.toStr adaBalance) <> " ₳"

-- | Get first address in Bech32 format
getUserFirstAddressBech32 :: forall m. MonadCIP30 m => Api -> m String
getUserFirstAddressBech32 api = do
  userAddresses <- getUsedAddresses api Nothing
  pure
    $ case userAddresses !! 0 of
        Just firstAddrHex -> case Csl.address.fromHex firstAddrHex of
          Just firstAddr -> Csl.address.toBech32 firstAddr Nothing
          Nothing -> ""
        Nothing -> ""

-- | Get available wallets with icons
getTheAvailableWallets :: forall m. MonadCIP30 m => m (Array (Tuple WalletName String))
getTheAvailableWallets = do
  ws <- getAvailableWallets
  is <- sequence $ map getIcon ws
  pure $ zip ws is
