import Simulation

main :: IO ()
main = do
    putStr "["
    mapM_ runSimWithSeed [1..5]
    putStr "]"

runSimWithSeed :: Int -> IO ()
runSimWithSeed seed = do
    parameters <- extractParams
    let simResults = computeSimulation parameters
    print simResults

extractParams :: IO AnalysisParameters
extractParams =
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
      }
