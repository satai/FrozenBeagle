import System.IO

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

periodLen :: Int
periodLen = 8 * 1024

startPeriodLen :: Int
startPeriodLen = 128


avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

draw :: (String, Int) -> String -> String -> IO()
draw (name, index) a b = do
    let
        fileName = a ++ "_" ++ b

    handle <- openFile ("result_" ++ fileName) ReadMode
    contents <- hGetContents handle

    let
        a = read contents :: [[(String, [(Integer, Double)])]]


        variable :: [[Double]]
        variable = map (map snd. (!! index) . map snd) a

        containsNaN = any isNaN

        period1 = map (take $ periodLen - 1) variable
        period2 = map (take (periodLen - 1) . drop (periodLen + 2)) variable
        period3 = map (drop $ 2 * periodLen + 1) variable

        extinct1 = map containsNaN period1
        extinct2 = map containsNaN period2
        extinct3 = map containsNaN period3

    print $ last $ last variable

    print $ "Extinct:"
    print $ show $ map containsNaN period1
    print $ show $ map containsNaN period2
    print $ show $ map containsNaN period3

    print $ "Extinct count:"
    print $ show $ length $ filter id extinct1
    print $ show $ length $ filter id extinct2
    print $ show $ length $ filter id extinct3

    print $ "Maximum " ++ name ++ ":"
    print $ show $ map (maximum . (0.0 : ))  period1
    print $ show $ map (maximum . (0.0 : ))  period2
    print $ show $ map (maximum . (0.0 : ))  period3

    print $ "Avg of maximums of " ++ name ++ ":"
    print $ show $ avg $ map (maximum . (0.0 : ))  period1
    print $ show $ avg $ map (maximum . (0.0 : ))  period2
    print $ show $ avg $ map (maximum . (0.0 : ))  period3

    print $ "Smernice zacatku  " ++ name ++ ":"
    print $ show $ map (\x -> (x !! startPeriodLen) / head x) period1

    toFile (fo_size .~ (3200,2600) $ def) (name ++ "_"  ++ fileName ++ ".png") $  mapM_ (\i -> plot $ points ("")  (map snd (a !! i) !! index)) [0..length a - 1]

