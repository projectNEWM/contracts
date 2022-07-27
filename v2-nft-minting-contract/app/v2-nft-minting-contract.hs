import Prelude
import Cardano.Api
import V2NFTMintingContract ( mintingPlutusScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-tokenized-minting-contract.plutus" Nothing mintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()