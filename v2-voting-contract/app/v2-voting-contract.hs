import Prelude
import Cardano.Api
import V2VotingContract ( votingContractScript)

main :: IO ()
main = do
  result <- writeFileTextEnvelope "v2-voting-contract.plutus" Nothing votingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()