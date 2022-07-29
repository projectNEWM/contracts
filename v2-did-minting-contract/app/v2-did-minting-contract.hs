import Prelude
import Cardano.Api
import V2DidMintingContract (didMintingPlutusScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-did-minting-contract.plutus" Nothing didMintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
