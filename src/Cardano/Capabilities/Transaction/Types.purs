-- | Transaction-related types for Cardano capabilities
module Cardano.Capabilities.Transaction.Types
  ( UserAddresses(..)
  , Interaction(..)
  , AddWitAndSubmitParams(..)
  , _UserAddresses
  , _Interaction
  , _AddWitAndSubmitParams
  ) where

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

-- | User wallet addresses for transaction building
newtype UserAddresses
  = UserAddresses
  { usedAddresses :: Array String
  , changeAddress :: String
  , stakeAddresses :: Array String
  }

instance encodeJsonUserAddresses :: EncodeJson UserAddresses where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions

instance decodeJsonUserAddresses :: DecodeJson UserAddresses where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions

derive instance genericUserAddresses :: Generic UserAddresses _
derive instance newtypeUserAddresses :: Newtype UserAddresses _

_UserAddresses ::
  Iso' UserAddresses
    { usedAddresses :: Array String
    , changeAddress :: String
    , stakeAddresses :: Array String
    }
_UserAddresses = _Newtype

--------------------------------------------------------------------------------

-- | Interaction data structure for transaction building
-- | Wraps an action with user addresses and optional recipient
newtype Interaction a
  = Interaction
  { action :: a
  , userAddresses :: UserAddresses
  , recipient :: Maybe String
  }

instance encodeJsonInteraction :: EncodeJson a => EncodeJson (Interaction a) where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions

instance decodeJsonInteraction :: (DecodeJson a, DecodeJsonField a) => DecodeJson (Interaction a) where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions

derive instance genericInteraction :: Generic (Interaction a) _
derive instance newtypeInteraction :: Newtype (Interaction a) _

_Interaction :: forall a. Iso' (Interaction a) { action :: a, userAddresses :: UserAddresses, recipient :: Maybe String }
_Interaction = _Newtype

--------------------------------------------------------------------------------

-- | Parameters for adding witness and submitting transaction
newtype AddWitAndSubmitParams =
  AddWitAndSubmitParams
    { tx_unsigned :: String
    , tx_wit :: String
    }

instance encodeJsonAddWitAndSubmitParams :: EncodeJson AddWitAndSubmitParams where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions

instance decodeJsonAddWitAndSubmitParams :: DecodeJson AddWitAndSubmitParams where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions

derive instance genericAddWitAndSubmitParams :: Generic AddWitAndSubmitParams _
derive instance newtypeAddWitAndSubmitParams :: Newtype AddWitAndSubmitParams _

_AddWitAndSubmitParams :: Iso' AddWitAndSubmitParams { tx_unsigned :: String, tx_wit :: String }
_AddWitAndSubmitParams = _Newtype

