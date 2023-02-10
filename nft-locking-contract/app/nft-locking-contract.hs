import Prelude
import Cardano.Api
import LockStarterNFTContract (lockingContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "nft-locking-contract.plutus" Nothing lockingContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()