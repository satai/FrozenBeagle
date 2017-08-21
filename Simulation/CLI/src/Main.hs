import Simulation

import Options.Applicative hiding ((<>))
import Data.Semigroup ((<>))

main :: IO ()
main = do
    let simCount = 3
    putStr "["
    runSimWithSeed False 0
    mapM_ (runSimWithSeed True) [1..(simCount - 1)]
    putStr "]"

runSimWithSeed :: Bool -> Int -> IO ()
runSimWithSeed prepend seedOffset = do
    putStr $ if prepend then "," else ""
    parameters <- execParser (info (extractParams) mempty)
    let simResults = computeSimulation $ parameters {seed = 1024 * (seed parameters + seedOffset)}
    putStr $ show simResults

extractParams :: Parser AnalysisParameters
extractParams = AnalysisParameters
                      <$> switch (long "separatedGenerations")
                      <*> option auto (long "hardSelectionThreshold" <> value 0.0)
                      <*> option auto (long "populationSize" <> value 300)
                      <*> option auto (long "optimumChange" <> value [])
                      <*> option auto (long "maxAge" <> value 64)
                      <*> option auto (long "countOfBases" <> value 20)
                      <*> option auto (long "countOfPleiotropicRules" <> value 0)
                      <*> option auto (long "countOfEpistaticRules" <> value 0)
                      <*> option auto (long "ratioOfNegativeDominantRules" <> value 0.0)
                      <*> option auto (long "ratioOfPositiveDominantRules" <> value 0.0)
                      <*> option auto (long "ratioOfPleiotropicRules" <> value 0.0)
                      <*> option auto (long "seed" <> value 0)
