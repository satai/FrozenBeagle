import Simulation

main :: IO ()
main = do
    putStr "["
    mapM_ runSimWithSeed [1..5]
    putStr "]"

runSimWithSeed :: Int -> IO ()
runSimWithSeed seedValue = do
    parameters <- extractParams seedValue
    let simResults = computeSimulation parameters
    print simResults

extractParams :: Int -> IO AnalysisParameters
extractParams seedValue =
    return AnalysisParameters {
           separatedGenerations = False
         , hardSelectionTreshold =  0.0
         , populationSize =  300
         , optimumChange = []
         , maxAge = 64
         , countOfBases = 50
         , countOfPleiotropicRules = 0
         , countOfEpistaticRules = 0
         , countOfComplicatedRules = 0
         , seed = seedValue
      }
