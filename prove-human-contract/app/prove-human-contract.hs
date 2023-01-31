import Prelude
import Cardano.Api
import ProveHumanContract ( proveHumanContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "prove-human-contract.plutus" Nothing proveHumanContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
