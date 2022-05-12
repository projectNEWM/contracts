{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module RoyaltyPayout
  ( royaltyPayoutScript
  , royaltyPayoutScriptShortBs
  , CustomDatumType
  , Schema
  , contract
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Value    as Value
import           Data.Aeson                ( FromJSON, ToJSON )
import           Data.OpenApi.Schema       ( ToSchema )
import           GHC.Generics              ( Generic )
import           Prelude                   ( Show )
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
  A lightweight smart contract solution for pure ADA group payouts.
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtGroupPKHs :: ![PubKeyHash]
    -- ^ List of the public key hashes
    , cdtPayouts   :: ![Integer]
    -- ^ List of the payouts in lovelace.
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType
-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data RoyaltyPayoutParams = RoyaltyPayoutParams {}
PlutusTx.makeLift ''RoyaltyPayoutParams
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType {}
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: TypeData -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: RoyaltyPayoutParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum _ context = oneScriptInput && checkAllPayments (cdtGroupPKHs datum) (cdtPayouts datum)
  where
    -- Get the Tx Info
    info :: TxInfo
    info = scriptContextTxInfo context

    -- Loop the pkh and amount lists, checking each case.
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments [] [] = True -- everyone got paid correctly
    checkAllPayments [] _  = True -- leftover goes back as change
    checkAllPayments _  [] = True -- leftover goes back as change
    checkAllPayments (pkh:pkhs) (amt:amts)
      | checkValuePaidTo = checkAllPayments pkhs amts
      | otherwise        = traceIfFalse "A Member Of The Group Is Not Being Paid." False
        where
          checkValuePaidTo :: Bool
          checkValuePaidTo = Value.geq (Contexts.valuePaidTo info pkh) (Ada.lovelaceValueOf amt)

    -- Force a single script utxo input.
    oneScriptInput :: Bool
    oneScriptInput = traceIfFalse "More Than One Script Input Is Being Validated." $ loopInputs (txInfoInputs info) 0
      where
        loopInputs :: [TxInInfo] -> Integer -> Bool
        loopInputs []      counter = counter == 1 -- There can only be one
        loopInputs (x:xs) !counter =
          case txOutDatumHash $ txInInfoResolved x of
            Nothing -> do counter <= 1 && loopInputs xs counter
            Just _  -> do counter <= 1 && loopInputs xs (counter + 1)
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType
-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: RoyaltyPayoutParams -> Scripts.TypedValidator Typed
typedValidator rpp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode rpp)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer
-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ RoyaltyPayoutParams {})
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

royaltyPayoutScriptShortBs :: SBS.ShortByteString
royaltyPayoutScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

royaltyPayoutScript :: PlutusScript PlutusScriptV1
royaltyPayoutScript = PlutusScriptSerialised royaltyPayoutScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract