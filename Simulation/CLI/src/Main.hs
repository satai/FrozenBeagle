import Simulation

import Options.Applicative
import Data.Semigroup ((<>))



main :: IO ()
main = do
    putStr "["
    mapM_ runSimWithSeed [1..5]
    putStr "]"

runSimWithSeed :: Int -> IO ()
runSimWithSeed seedValue = do
    parameters <- execParser (info (extractParams seedValue) mempty)
    let simResults = computeSimulation parameters
    print simResults

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
