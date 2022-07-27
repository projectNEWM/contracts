import           Prelude
import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import           NFTMintingContract (apiExamplePlutusMintingScript, mintingScriptShortBs)

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m mintingScriptShortBs [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "nft_minting_contract.plutus" Nothing apiExamplePlutusMintingScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
