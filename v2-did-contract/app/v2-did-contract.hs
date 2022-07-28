import Prelude
import Cardano.Api
import V2didContract ( didContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-did-contract.plutus" Nothing didContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()