{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Main where


import "gtk3" Graphics.UI.Gtk

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Concurrent
import System.Environment
import System.Directory

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

signal2 xs = [ (x,(cos (x*3.14159/45) + 1) / 2 * (cos (x*3.14159/5))) | x <- xs ]

data AnalysisParameters = AnalysisParameters {
    multipleRuns :: Bool,
    separatedGenerations :: Bool,
    populationSize :: Int
    }


data AnalysisParametersFields = AnalysisParametersFields {
    multipleRunsField :: CheckButton,
    separatedGenerationsField :: CheckButton,
    populationSizeField :: SpinButton
    }

disableButtonIfInBroadway :: Button -> IO()
disableButtonIfInBroadway button = do
    environment <- getEnvironment
    widgetSetSensitivity button (not $ runningInBroadway environment)
    return ()
    where
        runningInBroadway environment = (("GDK_BACKEND", "broadway") `elem` environment)


prepareWindow :: Entry -> AnalysisParametersFields -> Notebook -> IO(Window)
prepareWindow nameField parameterFields resultNotebook = do
    window  <- windowNew
    mainTable <- vBoxNew False 3

    settingsBox <- vBoxNew True 10

    settingsSwitchesBoxLeft <- vBoxNew False 0

    nameFieldBox <- hBoxNew False 10
    nameFieldlabel <- labelNew (Just "Description")
    boxPackStart nameFieldBox nameFieldlabel PackNatural 0
    boxPackStart nameFieldBox nameField PackNatural 10
    boxPackStart settingsSwitchesBoxLeft nameFieldBox PackNatural 0

    populationSizeBox <- hBoxNew False 10
    populationSizeLabel <- labelNew (Just "Population Size")

    boxPackStart populationSizeBox populationSizeLabel PackNatural 0
    boxPackStart populationSizeBox (populationSizeField parameterFields) PackNatural 10
    boxPackStart settingsSwitchesBoxLeft populationSizeBox PackNatural 0

    boxPackStart settingsSwitchesBoxLeft (separatedGenerationsField parameterFields) PackNatural 0
    boxPackStart settingsSwitchesBoxLeft (multipleRunsField parameterFields) PackNatural 0

    settingsSwitchesBoxRight <- vBoxNew False 0
    settingsSwitchesBox <- hBoxNew False 0
    boxPackStart settingsSwitchesBox settingsSwitchesBoxLeft PackNatural 0
    boxPackStart settingsSwitchesBox settingsSwitchesBoxRight PackNatural 0

    settingsLabel <- labelNew (Just "Settings:")
    miscSetAlignment settingsLabel 0 0
    boxPackStart settingsBox settingsLabel PackNatural 0
    boxPackStart settingsBox settingsSwitchesBox PackNatural 0

    runBox <- hBoxNew False 10
    runButton <- buttonNewWithLabel "Evolve!"
    boxPackEnd runBox runButton PackNatural 10

    boxPackStart mainTable settingsBox PackNatural 3
    sep1 <- hSeparatorNew

    boxPackStart mainTable runBox PackNatural 3
    boxPackStart mainTable sep1 PackNatural 3

    boxPackStart mainTable resultNotebook PackGrow 3
    Graphics.UI.Gtk.set resultNotebook [notebookScrollable := True, notebookTabPos := PosTop]

    sep2 <- hSeparatorNew
    boxPackStart mainTable sep2  PackNatural 3

    quitBox <- hBoxNew False 10
    quitbutton <- buttonNewFromStock stockQuit
    disableButtonIfInBroadway quitbutton

    boxPackEnd quitBox quitbutton PackNatural 10

    boxPackStart mainTable quitBox  PackNatural 3

    Graphics.UI.Gtk.set window [ windowDefaultWidth := 1600,
                                 windowDefaultHeight := 1200,
                                 containerBorderWidth := 10,
                                 containerChild := mainTable ]

    _ <- on runButton buttonActivated (runSimulation nameField parameterFields resultNotebook window)
    _ <- on quitbutton buttonActivated mainQuit
    _ <- on window objectDestroy mainQuit

    return (window)


main :: IO ()
main = do
    _ <- initGUI

    nameField <- entryNew
    separatedGenerationsSwitch <- checkButtonNewWithLabel "Separated Generations"
    multipleSimulationsSwitch <- checkButtonNewWithLabel "Multiple Simulations"
    populationSizeScale <- spinButtonNewWithRange  10 10000 10

    let parameterFields = AnalysisParametersFields separatedGenerationsSwitch multipleSimulationsSwitch populationSizeScale

    resultNotebook <- notebookNew

    window <- prepareWindow nameField parameterFields resultNotebook
    widgetShowAll window

    mainGUI

extractParameters :: AnalysisParametersFields -> IO (AnalysisParameters)
extractParameters parameterFields = do
    multipleRuns <- toggleButtonGetMode $ multipleRunsField parameterFields
    separatedGen <- toggleButtonGetMode $ separatedGenerationsField parameterFields
    popSize <- spinButtonGetValueAsInt $ populationSizeField parameterFields
    return (AnalysisParameters  multipleRuns separatedGen popSize)

runSimulation :: Entry -> AnalysisParametersFields -> Notebook -> Window -> IO()
runSimulation nameField parameterFields resultNotebook window =
    do
        parameters <- extractParameters parameterFields
        name <- entryGetText nameField
        _ <- forkIO $ do
            let simResults = [("t1", signal [0,7..400]), ("t2", signal2 [0,7..400])]
            putStrLn $ show simResults
            sequence_ (map (showResult resultNotebook name) simResults)
            
            postGUIAsync $ do
                widgetShowAll window
                return ()
        return ()

showResult resultNotebook name (resultName, resultValue) = do
            let fileName = "/tmp/img_" ++ name ++ "_" ++ resultName ++ ".png"

            toFile def fileName $ do
                layout_title .= (name ++ "/" ++ resultName)
                layout_all_font_styles . font_size %= (*1.5)
                plot (points resultName $ resultValue)

            putStrLn $ "Ploted to " ++ fileName
            postGUIAsync $ do
                drawingArea <- imageNew
                imageSetFromFile drawingArea fileName
                _ <- notebookAppendPage resultNotebook drawingArea (name ++ "/" ++ resultName)
                removeFile fileName
                widgetShowAll resultNotebook
                return ()
