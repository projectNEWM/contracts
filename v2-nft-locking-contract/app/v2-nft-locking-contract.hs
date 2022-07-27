import Prelude
import Cardano.Api
import V2NFTLockingContract (lockingContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-tokenized-locking-contract.plutus" Nothing lockingContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()