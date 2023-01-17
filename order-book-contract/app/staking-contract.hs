import           Prelude
import           Cardano.Api
import           StakeContract ( stakingPlutusScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "stake-contract.plutus" Nothing stakingPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
