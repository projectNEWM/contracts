import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short as SBS
import           LockingContract (lockingContractScript, lockingContractScriptShortBs)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args!!0) else 420
  let scriptname = if nargs > 1 then args!!1 else  "locking_contract.plutus"
  putStrLn $ "OUTPUT: " ++ scriptname
  writePlutusScript scriptnum scriptname lockingContractScript lockingContractScriptShortBs

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("OUTPUT" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Error" :: String) >> print evalErr
                  Right exbudget -> print ("Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
