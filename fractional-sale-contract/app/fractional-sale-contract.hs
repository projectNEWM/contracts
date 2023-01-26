import Prelude
import Cardano.Api
import FractionalSaleContract ( fractionalSaleContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "fractional-sale-contract.plutus" Nothing fractionalSaleContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
