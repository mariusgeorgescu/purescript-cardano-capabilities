-- | Capability type class for CIP-30 wallet interactions
-- |
-- | This module provides:
-- | - `MonadCIP30` - Type class abstracting CIP-30 wallet operations
-- | - Default instance for `HalogenM`
-- | - Utility functions for common wallet operations
module Cardano.Capabilities.Wallet.MonadCIP30
  ( class MonadCIP30
  , enable
  , getExtensions
  , getNetworkId
  , getUtxos
  , getCollateral
  , getBalance
  , getUsedAddresses
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , signTx
  , signData
  , submitTx
  , isEnabled
  , getAvailableWallets
  , getApiVersion
  , getName
  , getIcon
  , getSupportedExtensions
  , isWalletAvailable
  -- Utility functions
  , enableWallet
  , getUserAddresses
  , getUserFirstAddressBech32
  , getNetworkName
  , getNativeCoinBalance
  , getNativeCoinBalanceString
  , getNativeAssetsBalance
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
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H

-- | Type class for CIP-30 wallet operations
-- |
-- | Provides an abstract interface for interacting with Cardano wallets
-- | following the CIP-30 specification.
class Monad m <= MonadCIP30 m where
  -- | Enable a wallet with specified extensions
  enable :: WalletName -> Array Extension -> m Api
  
  -- | Get enabled extensions for the wallet
  getExtensions :: Api -> m (Array Extension)
  
  -- | Get the network ID (0 = testnet, 1 = mainnet)
  getNetworkId :: Api -> m NetworkId
  
  -- | Get UTxOs from the wallet
  getUtxos :: Api -> Maybe Cbor -> Maybe Paginate -> m (Maybe (Array Cbor))
  
  -- | Get collateral UTxOs
  getCollateral :: Api -> Cbor -> m (Maybe (Array Cbor))
  
  -- | Get the wallet balance as CBOR-encoded Value
  getBalance :: Api -> m Cbor
  
  -- | Get used addresses from the wallet
  getUsedAddresses :: Api -> Maybe Paginate -> m (Array Cbor)
  
  -- | Get unused addresses from the wallet
  getUnusedAddresses :: Api -> m (Array Cbor)
  
  -- | Get the change address
  getChangeAddress :: Api -> m Cbor
  
  -- | Get reward/stake addresses
  getRewardAddresses :: Api -> m (Array Cbor)
  
  -- | Sign a transaction
  signTx :: Api -> Cbor -> Boolean -> m Cbor
  
  -- | Sign arbitrary data
  signData :: Api -> Cbor -> Bytes -> m DataSignature
  
  -- | Submit a signed transaction
  submitTx :: Api -> Cbor -> m String
  
  -- | Check if wallet is enabled
  isEnabled :: WalletName -> m Boolean
  
  -- | Get list of available wallets
  getAvailableWallets :: m (Array WalletName)
  
  -- | Get wallet API version
  getApiVersion :: WalletName -> m String
  
  -- | Get wallet name
  getName :: WalletName -> m String
  
  -- | Get wallet icon (base64 encoded)
  getIcon :: WalletName -> m String
  
  -- | Get supported extensions for a wallet
  getSupportedExtensions :: WalletName -> m (Array Extension)
  
  -- | Check if wallet is available in the browser
  isWalletAvailable :: WalletName -> m Boolean

-- | Default instance for HalogenM
-- | Lifts CIP-30 operations into Halogen components
instance monadCip30HalogenM :: MonadAff m => MonadCIP30 (HalogenM st act slots msg m) where
  enable w exts = H.liftAff $ Cip30.enable w exts
  getExtensions = H.liftAff <<< Cip30.getExtensions
  getNetworkId = H.liftAff <<< Cip30.getNetworkId
  getUtxos api ma mp = H.liftAff $ Cip30.getUtxos api ma mp
  getCollateral api amt = H.liftAff $ Cip30.getCollateral api amt
  getBalance = H.liftAff <<< Cip30.getBalance
  getUsedAddresses api mp = H.liftAff $ Cip30.getUsedAddresses api mp
  getUnusedAddresses = H.liftAff <<< Cip30.getUnusedAddresses
  getChangeAddress = H.liftAff <<< Cip30.getChangeAddress
  getRewardAddresses = H.liftAff <<< Cip30.getRewardAddresses
  signTx api tx isPartial = H.liftAff $ Cip30.signTx api tx isPartial
  signData api addr payload = H.liftAff $ Cip30.signData api addr payload
  submitTx api tx = H.liftAff $ Cip30.submitTx api tx
  isEnabled = H.liftAff <<< Cip30.isEnabled
  getAvailableWallets = H.liftEffect Cip30.getAvailableWallets
  getApiVersion = H.liftEffect <<< Cip30.getApiVersion
  getName = H.liftEffect <<< Cip30.getName
  getIcon = H.liftEffect <<< Cip30.getIcon
  getSupportedExtensions = H.liftEffect <<< Cip30.getSupportedExtensions
  isWalletAvailable = H.liftEffect <<< Cip30.isWalletAvailable

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Enable a wallet with CIP-30 extension
enableWallet :: forall m. MonadCIP30 m => WalletName -> m Api
enableWallet wname = enable wname [ { cip: 30 } ]

-- | Get user addresses (convenience wrapper)
getUserAddresses :: forall m. MonadCIP30 m => Api -> m (Array String)
getUserAddresses api = getUsedAddresses api Nothing

-- | Get the first user address in bech32 format
getUserFirstAddressBech32 :: forall m. MonadCIP30 m => Api -> m String
getUserFirstAddressBech32 api = do
  userAddresses <- getUserAddresses api
  pure
    $ case userAddresses !! 0 of
        Just firstAddrHex -> case Csl.address.fromHex firstAddrHex of
          Just firstAddr -> Csl.address.toBech32 firstAddr Nothing
          Nothing -> ""
        Nothing -> ""

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

-- | Get native coin (ADA) balance in lovelace
getNativeCoinBalance :: forall m. MonadCIP30 m => Api -> m Csl.BigNum
getNativeCoinBalance api = do
  walletBalanceCbor <- getBalance api
  let
    balance = fromMaybe Csl.value.zero $ Csl.value.fromHex walletBalanceCbor
  pure $ Csl.bigNum.divFloor (Csl.value.coin balance) (maybe Csl.bigNum.one identity (Csl.bigNum.fromStr "1000000"))

-- | Get native coin balance as formatted string with ADA symbol
getNativeCoinBalanceString :: forall m. MonadCIP30 m => Api -> m String
getNativeCoinBalanceString api = do
  adaBalance <- getNativeCoinBalance api
  pure $ (Csl.bigNum.toStr adaBalance) <> " ₳"

-- | Get native assets (tokens) balance
getNativeAssetsBalance :: forall m. MonadCIP30 m => Api -> m (Maybe Csl.MultiAsset)
getNativeAssetsBalance api = do
  walletBalanceCbor <- getBalance api
  let
    balance = fromMaybe Csl.value.zero $ Csl.value.fromHex walletBalanceCbor
  pure $ Csl.value.multiasset balance

-- | Get available wallets with their icons
getTheAvailableWallets :: forall m. MonadCIP30 m => m (Array (Tuple WalletName String))
getTheAvailableWallets = do
  ws <- getAvailableWallets
  is <- sequence $ map getIcon ws
  pure $ zip ws is

