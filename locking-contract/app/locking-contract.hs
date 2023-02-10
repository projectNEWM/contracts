import Prelude
import Cardano.Api
import LockTokenizedNFTContract ( lockingContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "locking-contract.plutus" Nothing lockingContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()