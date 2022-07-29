import Prelude
import Cardano.Api
import V2DidLockingContract ( didLockingContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-did-locking-contract.plutus" Nothing didLockingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()