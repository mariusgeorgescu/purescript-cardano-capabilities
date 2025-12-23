-- | Capability type class for CIP-30 wallet interactions
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
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Api, Bytes, Cbor, DataSignature, Extension, NetworkId, Paginate, WalletName)
import Cardano.Wallet.Cip30 as Cip30
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H

-- | Capability for CIP-30 wallet interactions
-- | 
-- | This type class abstracts over the CIP-30 wallet API, allowing
-- | different implementations for production, testing, etc.
class Monad m <= MonadCIP30 m where
  -- | Enable a wallet with optional extensions
  enable :: WalletName -> Array Extension -> m Api
  
  -- | Get enabled extensions for a wallet API
  getExtensions :: Api -> m (Array Extension)
  
  -- | Get the network ID (0 = testnet, 1 = mainnet)
  getNetworkId :: Api -> m NetworkId
  
  -- | Get wallet balance as CBOR-encoded value
  getBalance :: Api -> m Cbor
  
  -- | Get used addresses with optional pagination
  getUsedAddresses :: Api -> Maybe Paginate -> m (Array Cbor)
  
  -- | Get UTxOs with optional amount filter and pagination
  getUtxos :: Api -> Maybe Cbor -> Maybe Paginate -> m (Maybe (Array Cbor))
  
  -- | Get collateral UTxOs for a given amount
  getCollateral :: Api -> Cbor -> m (Maybe (Array Cbor))
  
  -- | Get unused addresses
  getUnusedAddresses :: Api -> m (Array Cbor)
  
  -- | Get change address
  getChangeAddress :: Api -> m Cbor
  
  -- | Get reward/stake addresses
  getRewardAddresses :: Api -> m (Array Cbor)
  
  -- | Sign a transaction (partial signing supported)
  signTx :: Api -> Cbor -> Boolean -> m Cbor
  
  -- | Sign arbitrary data
  signData :: Api -> Cbor -> Bytes -> m DataSignature
  
  -- | Submit a signed transaction
  submitTx :: Api -> Cbor -> m String
  
  -- | Get wallet name
  getName :: WalletName -> m String
  
  -- | Get wallet icon
  getIcon :: WalletName -> m String
  
  -- | Check if a wallet is available
  isWalletAvailable :: WalletName -> m Boolean
  
  -- | Get list of available wallets
  getAvailableWallets :: m (Array WalletName)
  
  -- | Check if a wallet is enabled
  isEnabled :: WalletName -> m Boolean
  
  -- | Get wallet API version
  getApiVersion :: WalletName -> m String
  
  -- | Get supported extensions for a wallet
  getSupportedExtensions :: WalletName -> m (Array Extension)

-- | Lift MonadCIP30 into HalogenM so components can call without manual lift
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
