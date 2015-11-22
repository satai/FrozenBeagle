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

labeledNewTextField :: String -> VBox -> IO(Entry)
labeledNewTextField description container = do
    entry <- entryNew
    insertIntoBoxWithLabel entry description container
    return entry

labeledNewScale :: String -> Double -> Double -> Double -> VBox -> IO(SpinButton)
labeledNewScale description min max step container = do
    scale <- spinButtonNewWithRange min max step
    insertIntoBoxWithLabel scale description container
    return scale

checkBoxNewWithLabel :: String -> VBox -> IO(CheckButton)
checkBoxNewWithLabel description container = do
    switch <- checkButtonNew
    insertIntoBoxWithLabel switch description container
    return switch

insertIntoBoxWithLabel control description container = do
    box <- hBoxNew False 10
    label <- labelNew (Just description)

    boxPackStart box label PackNatural 0
    boxPackEnd box control  PackNatural 10
    boxPackStart container box PackNatural 0

prepareWindow :: Notebook -> IO(Window)
prepareWindow resultNotebook = do

    window  <- windowNew
    mainTable <- vBoxNew False 3

    settingsBox <- vBoxNew True 10
    settingsSwitchesBoxLeft <- vBoxNew False 0

    nameField <- labeledNewTextField "Description" settingsSwitchesBoxLeft
    populationSizeScale <- labeledNewScale "Population Size" 10 1000 10 settingsSwitchesBoxLeft
    hardSelectionTresholdScale <- labeledNewScale "Hard selection treshold" 0.0 100.0 0.01 settingsSwitchesBoxLeft
    separatedGenerationsSwitch <- checkBoxNewWithLabel "Separated Generations" settingsSwitchesBoxLeft

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
