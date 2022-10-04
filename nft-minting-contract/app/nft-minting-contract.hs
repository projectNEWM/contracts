import Prelude
import Cardano.Api
import NFTMintingContract ( mintingPlutusScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "nft-minting-contract.plutus" Nothing mintingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()