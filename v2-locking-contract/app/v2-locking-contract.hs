import Prelude
import Cardano.Api
import V2LockingContract ( lockingContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-fractional-locking-contract.plutus" Nothing lockingContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()