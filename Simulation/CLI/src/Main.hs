import Simulation

import Options.Applicative hiding ((<>))
import Data.Semigroup ((<>))

main :: IO ()
main = do
    putStr "["
    runSimWithSeed False 0
    mapM_ (runSimWithSeed True) [1..5]
    putStr "]"

runSimWithSeed :: Bool -> Int -> IO ()
runSimWithSeed prepend seedValue = do
    putStr $ if prepend then "," else ""
    parameters <- execParser (info (extractParams seedValue) mempty)
    let simResults = computeSimulation parameters
    putStr $ show simResults

extractParams :: Int -> Parser AnalysisParameters
extractParams seedValue = AnalysisParameters
                      <$> switch (long "separatedGenerations")
                      <*> option auto (long "hardSelectionTreshold" <> value 0.0)
                      <*> option auto (long "populationSize" <> value 300)
                      <*> option auto (long "optimumChange" <> value [])
                      <*> option auto (long "maxAge" <> value 64)
                      <*> option auto (long "countOfBases" <> value 50)
                      <*> option auto (long "countOfPleiotropicRules" <> value 0)
                      <*> option auto (long "countOfEpistaticRules" <> value 0)
                      <*> option auto (long "countOfComplicatedRules" <> value 0)
                      <*> option auto (long "dont_use" <> value seedValue)
