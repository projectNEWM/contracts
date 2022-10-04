import Prelude
import Cardano.Api
import MarketplaceContract ( marketplaceContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "marketplace-contract.plutus" Nothing marketplaceContractScript 
  case result of
    Left err -> print $ displayError err
    Right () -> return ()