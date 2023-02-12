{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
import Prelude
import Cardano.Api
import Data.Aeson
import GHC.Generics
import System.Directory
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import qualified Data.ByteString.Lazy  as BS
import LockStarterNFTContract          ( lockingContractScript, ScriptParameters(..) )
import Data.ByteString.Lazy            ( ByteString )
import qualified UsefulFuncs           ( createBuiltinByteString )
-- import Data.ByteString.Lazy.Char8      ( unpack )

-- Define a type for the JSON data
data MyData = MyData
  { pkh       :: [Integer]
  , multisig1 :: [Integer]
  , multisig2 :: [Integer]
  , multisig3 :: [Integer]
  , pid       :: [Integer]
  , tkn       :: [Integer]
  } deriving (Show, Generic)

instance FromJSON MyData

-- Read the JSON file into a ByteString
readJsonFile :: FilePath -> IO ByteString
readJsonFile filePath = BS.readFile filePath

-- Parse the ByteString into a MyData value
parseJson :: ByteString -> Maybe MyData
parseJson = decode

  -- putStrLn $ "JSON Data: " ++ (unpack jsonData)
main :: IO ()
main = do
  let filePath = "nft_locking_info.json"
  cwd <- getCurrentDirectory
  putStrLn $ "Current working directory: " ++ cwd
  putStrLn $ "Reading file: " ++ filePath
  jsonData <- readJsonFile filePath
  case parseJson jsonData of
    Just myData ->
      let delegator = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString $ pkh myData }
          multi1    = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString $ multisig1 myData }
          multi2    = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString $ multisig2 myData }
          multi3    = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString $ multisig3 myData }
          sPid      = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString $ pid myData }
          sTkn      = PlutusV2.TokenName { PlutusV2.unTokenName = UsefulFuncs.createBuiltinByteString $ tkn myData }
          sp        = ScriptParameters { starterPid = sPid
                                       , starterTkn = sTkn
                                       , mainPkh    = delegator
                                       , multiPkhs  = [multi1, multi2, multi3]
                                       }
      in do
        result <- writeFileTextEnvelope "nft-locking-contract.plutus" Nothing (lockingContractScript sp) 
        case result of
          Left err -> print $ displayError err
          Right () -> return ()
    Nothing -> putStrLn "Failed to parse JSON data"
