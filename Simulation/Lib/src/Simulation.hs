module Simulation(computeSimulation, AnalysisParameters(..),
                  pleiotropicRules, randomPleiotropicRule, turbidostatCoefiecientsForPopulationSize) where

import Evolution
import Expression
import Schema
import Genes
import Phenotype
import Population

import Data.MultiSet(toOccurList, fromList)
import Data.List

import Data.Random
import Data.Random.Extras
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal
import System.Random


data AnalysisParameters = AnalysisParameters {
    separatedGenerations :: Bool,
    hardSelectionTreshold :: Double,
    populationSize :: Int,
    optimumChange :: [(Double, Double, Double)],
    maxAge :: Int,
    countOfBases :: Int,
    countOfPleiotropicRules :: Int,
    countOfEpistaticRules :: Int,
    countOfComplicatedRules :: Int
    } deriving Show

randomPopulation :: Int -> ExpressionStrategy -> Int -> RVar Population
randomPopulation count expressionStrategy baseCount = Population 0 <$> randomIndividuals count expressionStrategy baseCount

randomIndividuals :: Int -> ExpressionStrategy -> Int -> RVar [Individual]
randomIndividuals count expressionStrategy baseCount = sequence $ replicate count $ randomIndividual baseCount expressionStrategy

randomIndividual :: Int -> ExpressionStrategy -> RVar Individual
randomIndividual baseCount expressionStrategy = do
        gender <- randomGender
        chs <- randomChromosomes baseCount
        return $ Individual gender 0 chs $ expressionStrategy gender chs

randomGender :: RVar Sex
randomGender = choice [F, M]

randomChromosomes :: Int -> RVar (DnaString, DnaString)
randomChromosomes baseCount = do
            dna1 <- randomDnaString baseCount
            dna2 <- randomDnaString baseCount
            return (dna1, dna2)

randomDnaString :: Int -> RVar DnaString
randomDnaString baseCount = DnaString <$> sequence (replicate baseCount randomBase)

randomBase :: RVar Basis
randomBase = choice [G1, G2, G3, G4, G5]

avgFitness :: (Int -> Phenotype) -> Int -> Population -> Double
avgFitness optimumForGeneration generation = avgFitnessForGeneration (optimumForGeneration generation)

avgFitnessForGeneration :: Phenotype -> Population -> Double
avgFitnessForGeneration optimum (Population _ is)  = average $ map (fitness optimum . phenotype) is

stdDevFitness :: (Int -> Phenotype) -> Int -> Population -> Double
stdDevFitness optimumForGeneration generation = stdDevFitnessForGeneration (optimumForGeneration generation)

stdDevFitnessForGeneration :: Phenotype -> Population -> Double
stdDevFitnessForGeneration optimum (Population _ is)  = stdDev $ map (fitness optimum . phenotype) is

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

almostAllTheSame :: (Ord a) => [a] -> Bool
almostAllTheSame xs = (0.90 :: Double) * fromIntegral (length xs)  >= fromIntegral (maximum $ map snd $ toOccurList $ fromList xs)

polymorphism :: Population -> Double
polymorphism population = 1.0 - (fromIntegral $ length $ filter id same) / (fromIntegral $ length same)
    where
      chs = map chromosomes $ individuals population
      genesList = transpose $ map genes $ map fst chs ++ map snd chs
      same :: [Bool]
      same = map allTheSame genesList

almostPolymorphism :: Population -> Double
almostPolymorphism population = 1.0 - (fromIntegral $ length $ filter id same) / (fromIntegral $ length same)
    where
      chs = map chromosomes $ individuals population
      genesList = transpose $ map genes $ map fst chs ++ map snd chs
      same :: [Bool]
      same = map almostAllTheSame genesList

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev xs = sqrt $ summedElements / count
  where
    avg = average xs
    count = fromIntegral $ length xs
    summedElements = sum (map (\x -> (x - avg) * (x - avg)) xs)

minFitness :: (Int -> Phenotype) -> Int -> Population -> Double
minFitness optimumForGeneration generation = minFitnessForGeneration (optimumForGeneration generation)

minFitnessForGeneration :: Phenotype -> Population -> Double
minFitnessForGeneration _       (Population _ []) = 1.0 / 0.0
minFitnessForGeneration optimum (Population _ is) = minimum $ map (fitness optimum . phenotype) is

percentileFitness :: Double -> (Int -> Phenotype) -> Int -> Population -> Double
percentileFitness _          _                    _           (Population _ []) = 1.0 / 0.0
percentileFitness percentile optimumForGeneration generation  (Population _ is) = (sort $ map (fitness (optimumForGeneration generation) . phenotype) is) !! (floor $ percentile * (fromIntegral $ length is))

randomRules :: Int -> Int -> Int -> Int -> RVar [(Schema, Phenotype)]
randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount = do
  br <- basicRules baseCount
  er <- epistaticRules baseCount pleiotropicRulesCount
  pr <- pleiotropicRules baseCount epistaticRulesCount
  cr <- complicatedRules baseCount complicatedRulesCount
  return (br ++ pr ++ er ++ cr)

epistaticRules :: Int -> Int -> RVar [(Schema, Phenotype)]
epistaticRules baseCount countOfRules = sequence $ take countOfRules $ repeat (randomEpistaticRule baseCount)

pleiotropicRules :: Int -> Int -> RVar [(Schema, Phenotype)]
pleiotropicRules baseCount countOfRules = sequence $ take countOfRules $ repeat (randomPleiotropicRule baseCount)

randomPleiotropicRule :: Int -> RVar (Schema, Phenotype)
randomPleiotropicRule baseCount = do
    g1Change <- doubleStdNormal
    g2Change <- doubleStdNormal
    g3Change <- doubleStdNormal
    g4Change <- doubleStdNormal

    position <- integralUniform 0 (baseCount - 1)

    basis <- choice [G1, G2, G3, G4, G5]

    let dimChange = [g1Change / 8.0, g2Change / 8.0, g3Change / 8.0, g4Change / 8.0]
    let schema = replicate position Nothing ++ [Just basis] ++ replicate (baseCount - position - 1) Nothing

    return (Schema schema, Phenotype dimChange)

basicRules :: Int -> RVar [(Schema, Phenotype)]
basicRules baseCount = concat <$> sequence (map (simpleRulesForPosition baseCount) [0..(baseCount - 1)])

complicatedRules :: Int -> Int -> RVar [(Schema, Phenotype)]
complicatedRules baseCount countOfRules = sequence $ take countOfRules $ repeat (randomComplicatedRule baseCount)

simpleRulesForPosition :: Int -> Int -> RVar [(Schema, Phenotype)]
simpleRulesForPosition baseCount p = do
    dimension <- integralUniform 0 3

    g1Change <- doubleStdNormal
    g2Change <- doubleStdNormal
    g3Change <- doubleStdNormal
    g4Change <- doubleStdNormal
    g5Change <- doubleStdNormal

    let g1DimChange = replicate dimension 0.0 ++ [g1Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g2DimChange = replicate dimension 0.0 ++ [g2Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g3DimChange = replicate dimension 0.0 ++ [g3Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g4DimChange = replicate dimension 0.0 ++ [g4Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g5DimChange = replicate dimension 0.0 ++ [g5Change / 8.0] ++ replicate (4 - dimension - 1) 0.0

    let schema1 = replicate p Nothing ++ [Just G1] ++ replicate (baseCount - p - 1) Nothing
    let schema2 = replicate p Nothing ++ [Just G2] ++ replicate (baseCount - p - 1) Nothing
    let schema3 = replicate p Nothing ++ [Just G3] ++ replicate (baseCount - p - 1) Nothing
    let schema4 = replicate p Nothing ++ [Just G4] ++ replicate (baseCount - p - 1) Nothing
    let schema5 = replicate p Nothing ++ [Just G5] ++ replicate (baseCount - p - 1) Nothing

    return [
        (Schema schema1, Phenotype g1DimChange),
        (Schema schema2, Phenotype g2DimChange),
        (Schema schema3, Phenotype g3DimChange),
        (Schema schema4, Phenotype g4DimChange),
        (Schema schema5, Phenotype g5DimChange)
      ]

randomEpistaticRule :: Int -> RVar (Schema, Phenotype)
randomEpistaticRule baseCount = do
    schema <- randomSchema baseCount
    dimension <- integralUniform 0 3
    gChange <- doubleStdNormal

    let gDimChange = replicate dimension 0.0 ++ [gChange / 8.0] ++ replicate (4 - dimension - 1) 0.0

    return (schema, Phenotype gDimChange)

randomComplicatedRule :: Int -> RVar (Schema, Phenotype)
randomComplicatedRule baseCount = do
    schema <- randomSchema baseCount
    p <- randomPhenotypeChange
    return (schema, p)

randomSchema :: Int -> RVar Schema
randomSchema baseCount = Schema <$> sequence elems
        where
            elems :: [RVar (Maybe Basis)]
            elems =  take baseCount $ repeat randomBaseOrNot -- fixme

randomBaseOrNot :: RVar (Maybe Basis)
randomBaseOrNot = choice $ [Just G1, Just G2, Just G3, Just G4] ++ replicate 20 Nothing  -- fixme

randomPhenotypeChange :: RVar Phenotype
randomPhenotypeChange = do
        a1 <- doubleStdUniform
        a2 <- doubleStdUniform
        a3 <- doubleStdUniform
        a4 <- doubleStdUniform
        return $ Phenotype [a1, a2, a3, a4]

express :: Int -> Int -> Int -> Int -> ExpressionStrategy
express baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount = schemaBasedExpression $ fst $ sampleState (randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount ) (mkStdGen 0)

colapse :: RVar a -> a
colapse x = fst $ sampleState x (mkStdGen 0)

turbidostatCoefiecientsForPopulationSize :: Double -> Int -> Double
turbidostatCoefiecientsForPopulationSize accidentDeathProbability expectedPopulationSize =
      (0.5 - accidentDeathProbability) / fromIntegral expectedPopulationSize / fromIntegral expectedPopulationSize

params2rules :: AnalysisParameters -> EvolutionRules
params2rules params =
    let baseCount = countOfBases params

        pleiotropicRulesCount = countOfPleiotropicRules params
        epistaticRulesCount = countOfEpistaticRules params
        complicatedRulesCount = countOfComplicatedRules params

        expression' = express baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount

        breedingStrategy = if separatedGenerations params
                                  then panmictic expression'
                                  else panmicticOverlap expression'

        startPopulationSize = populationSize params

        hSelection :: Phenotype -> Selection
        hSelection optimum = hardSelection (fitness optimum) $ hardSelectionTreshold params

        maximumAge = maxAge params

        accidentDeathProbability = 0.0
    in
        EvolutionRules {
                           mutation = [ pointMutation expression' ],
                           breeding = [ breedingStrategy ],
                           selection = [ hSelection ],
                           deaths = [
                                \_ -> turbidostat (turbidostatCoefiecientsForPopulationSize accidentDeathProbability (2 * startPopulationSize)) accidentDeathProbability,
                                \g -> killOld maximumAge g
                           ],
                           expression = expression',
                           optimumForGeneration = \g -> if (g < 1200) then Phenotype [1.0, 0.0, 0.0, 0.0] else Phenotype [0.8, 0.2, 0.0, 0.0]
                      }

maxSteps :: Int
maxSteps = 2500

computeSimulation :: AnalysisParameters -> [(String, [(Integer, Double)])]
computeSimulation params =
    let rules = params2rules params
        startPopulationSize = populationSize params

        initialPopulation = randomPopulation startPopulationSize (expression rules) $ countOfBases params
        allGenerations = evolution maxSteps rules initialPopulation
        generations = colapse allGenerations

        stats f = zip [0..] (zipWith f [0..] generations)

    in
        [
            ("Avg Fitness", stats $ avgFitness $ optimumForGeneration rules)
          , ("Min Fitness", stats $ minFitness $ optimumForGeneration rules)
          , ("10% percentile Fitness", stats $ percentileFitness 0.1 $ optimumForGeneration rules)
          , ("Ochylka Fitness", stats $ stdDevFitness $ optimumForGeneration rules)
          , ("Population Size", stats (\_ -> fromIntegral . length . individuals))
          , ("% of polymorphic locus", stats (\_ -> polymorphism) )
          , ("% of locus with alela with more than 90% appearence", stats (\_ -> almostPolymorphism))
        ]       -- fixme
