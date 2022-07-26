import Prelude
import Cardano.Api
import V2MintingContract (mintingPlutusScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-fractional-minting-contract.plutus" Nothing mintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
