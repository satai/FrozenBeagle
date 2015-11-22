{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Main where


import "gtk3" Graphics.UI.Gtk

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Concurrent
import System.Environment
import System.Directory

import Simulation

data AnalysisParametersFields = AnalysisParametersFields {
    separatedGenerationsField :: CheckButton,
    hardSelectionTresholdField :: SpinButton,
    populationSizeField :: SpinButton
    }

disableButtonIfInBroadway :: Button -> IO()
disableButtonIfInBroadway button = do
    environment <- getEnvironment
    widgetSetSensitivity button (not $ runningInBroadway environment)
    return ()
    where
        runningInBroadway environment = (("GDK_BACKEND", "broadway") `elem` environment)

labeledTextField :: String -> VBox -> IO(Entry)
labeledTextField description container = do
    entry <- entryNew

    nameFieldBox <- hBoxNew False 10
    nameFieldlabel <- labelNew (Just description)
    boxPackStart nameFieldBox nameFieldlabel PackNatural 0
    boxPackStart nameFieldBox entry PackNatural 10
    boxPackStart container nameFieldBox PackNatural 0
    return entry

prepareWindow :: Notebook -> IO(Window)
prepareWindow resultNotebook = do

    separatedGenerationsSwitch <- checkButtonNewWithLabel "Separated Generations"
    hardSelectionTresholdScale <- spinButtonNewWithRange 0 100 0.001
    populationSizeScale <- spinButtonNewWithRange  10 10000 10

    window  <- windowNew
    mainTable <- vBoxNew False 3

    settingsBox <- vBoxNew True 10

    settingsSwitchesBoxLeft <- vBoxNew False 0

    nameField <- labeledTextField "Description" settingsSwitchesBoxLeft

    populationSizeBox <- hBoxNew False 10
    populationSizeLabel <- labelNew (Just "Population Size")

    hardSelectionTresholdBox <- hBoxNew False 10
    hardSelectionTresholdLabel <- labelNew (Just "Hard Selection Treshold")

    boxPackStart populationSizeBox populationSizeLabel PackNatural 0
    boxPackStart populationSizeBox populationSizeScale  PackNatural 10
    boxPackStart settingsSwitchesBoxLeft populationSizeBox PackNatural 0

    boxPackStart hardSelectionTresholdBox hardSelectionTresholdLabel PackNatural 0
    boxPackStart hardSelectionTresholdBox hardSelectionTresholdScale PackNatural 10
    boxPackStart settingsSwitchesBoxLeft hardSelectionTresholdBox PackNatural 0

    boxPackStart settingsSwitchesBoxLeft separatedGenerationsSwitch PackNatural 0

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

    Graphics.UI.Gtk.windowFullscreen window

    let inputs = AnalysisParametersFields separatedGenerationsSwitch hardSelectionTresholdScale populationSizeScale

    _ <- on runButton buttonActivated (runSimulation nameField inputs resultNotebook window)
    _ <- on quitbutton buttonActivated mainQuit
    _ <- on window objectDestroy mainQuit


    return (window)


main :: IO ()
main = do
    _ <- initGUI

    resultNotebook <- notebookNew

    window <- prepareWindow resultNotebook
    widgetShowAll window

    mainGUI

extractParameters :: AnalysisParametersFields -> IO (AnalysisParameters)
extractParameters parameterFields = do
    separatedGen <- toggleButtonGetMode $ separatedGenerationsField parameterFields
    popSize <- spinButtonGetValueAsInt $ populationSizeField parameterFields
    hardSelectionTreshold <- spinButtonGetValue $ hardSelectionTresholdField parameterFields
    return (AnalysisParameters separatedGen hardSelectionTreshold popSize)

runSimulation :: Entry -> AnalysisParametersFields -> Notebook -> Window -> IO()
runSimulation nameField parameterFields resultNotebook window =
    do
        parameters <- extractParameters parameterFields
        name <- entryGetText nameField
        _ <- forkIO $ do
            let simResults = computeSimulation parameters
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
