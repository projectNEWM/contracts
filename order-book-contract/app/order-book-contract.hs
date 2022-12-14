import           Prelude
import           Cardano.Api
import           OrderBookContract ( orderBookContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "order-book-contract.plutus" Nothing orderBookContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
