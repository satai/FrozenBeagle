{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Main where


import "gtk3" Graphics.UI.Gtk

import Data.Maybe
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
    populationSizeField :: SpinButton,
    optimumMovementFields :: [(SpinButton, SpinButton, SpinButton)]
    }

disableButtonIfInBroadway :: Button -> IO()
disableButtonIfInBroadway button = do
    environment <- getEnvironment
    widgetSetSensitivity button (not $ runningInBroadway environment)
    return ()
    where
        runningInBroadway environment = ("GDK_BACKEND", "broadway") `elem` environment

labeledNewTextField :: String -> VBox -> IO Entry
labeledNewTextField description container = do
    entry <- entryNew
    insertIntoBoxWithLabel entry description container
    return entry

labeledNewScale :: String -> Double -> Double -> Double -> VBox -> IO SpinButton
labeledNewScale description min max step container = do
    scale <- spinButtonNewWithRange min max step
    insertIntoBoxWithLabel scale description container
    return scale

checkBoxNewWithLabel :: String -> VBox -> IO CheckButton
checkBoxNewWithLabel description container = do
    switch <- checkButtonNew
    insertIntoBoxWithLabel switch description container
    return switch

insertIntoBoxWithLabel control description container = do
    box <- hBoxNew False 10
    label <- labelNew (Just description)

    boxPackStart box label PackNatural 3
    boxPackEnd box control  PackNatural 3
    boxPackStart container box PackNatural 0

placeAndSizeWindow window mainTable = do
    Graphics.UI.Gtk.set window [ windowDefaultWidth := 1600,
                                 windowDefaultHeight := 1200,
                                 containerBorderWidth := 10,
                                 containerChild := mainTable ]

    Graphics.UI.Gtk.windowFullscreen window

optimumMovement row container = do
  hbox <- hBoxNew False 10

  label <- labelNew $ Just $ "Dimension " ++ show row
  boxPackStart hbox label PackNatural 3

  period <- spinButtonNewWithRange 0 200 1
  spinButtonSetValue period 200
  boxPackStart hbox period PackNatural 3
  widgetSetTooltipText period (Just "Period in generations")

  amplitude <- spinButtonNewWithRange 0 10.0 0.001
  spinButtonSetValue amplitude 0.0
  boxPackStart hbox amplitude PackNatural 3
  widgetSetTooltipText amplitude (Just "Amplitude")

  gradient <- spinButtonNewWithRange 0 1.0 0.001
  spinButtonSetValue gradient 0.0
  boxPackStart hbox gradient PackNatural 3
  widgetSetTooltipText gradient (Just "Gradient of change - change per generation")

  boxPackStart container hbox PackNatural 3
  return (period, amplitude, gradient)

optimumMovements container =
   mapM (`optimumMovement` container) [1..4]

prepareWindow :: IO Window
prepareWindow = do

    window  <- windowNew
    mainTable <- vBoxNew False 3

    settingsBox <- vBoxNew False 10

    settingsSwitchesBoxLeft <- vBoxNew False 0
    containerSetBorderWidth settingsSwitchesBoxLeft 10
    boxSetHomogeneous settingsSwitchesBoxLeft True

    nameField <- labeledNewTextField "Description" settingsSwitchesBoxLeft

    populationSizeScale <- labeledNewScale "Population Size" 10 99999 50 settingsSwitchesBoxLeft
    _ <- spinButtonSetValue populationSizeScale 300

    hardSelectionTresholdScale <- labeledNewScale "Hard Selection Treshold" 0.0 1000.0 0.01 settingsSwitchesBoxLeft
    separatedGenerationsSwitch <- checkBoxNewWithLabel "Separated Generations" settingsSwitchesBoxLeft

    _ <- toggleButtonSetActive separatedGenerationsSwitch False

    settingsSwitchesBoxRight <- vBoxNew False 0
    containerSetBorderWidth settingsSwitchesBoxRight 10
    settingsSwitchesBox <- hBoxNew False 0
    boxSetHomogeneous settingsSwitchesBoxRight True

    settingsSwitchesFrameLeft <- frameNew
    containerAdd settingsSwitchesFrameLeft settingsSwitchesBoxLeft

    settingsSwitchesFrameRight <- frameNew
    containerAdd settingsSwitchesFrameRight settingsSwitchesBoxRight

    boxPackStart settingsSwitchesBox settingsSwitchesFrameLeft PackNatural 30
    boxPackStart settingsSwitchesBox settingsSwitchesFrameRight PackNatural 30

    optimumMovements <- optimumMovements settingsSwitchesBoxRight

    settingsLabel <- labelNew (Just "Settings:")
    miscSetAlignment settingsLabel 0 0
    boxPackStart settingsBox settingsLabel PackNatural 3
    boxPackStart settingsBox settingsSwitchesBox PackNatural 3

    runBox <- hBoxNew False 10

    runButton <- buttonNewWithLabel "Evolve!"
    boxPackEnd runBox runButton PackNatural 3

    resetButton <- buttonNewWithLabel "Reset Settings"
    boxPackEnd runBox resetButton PackNatural 3

    boxPackStart mainTable settingsBox PackNatural 3
    sep1 <- hSeparatorNew

    boxPackStart mainTable runBox PackNatural 3
    boxPackStart mainTable sep1 PackNatural 3

    resultNotebook <- notebookNew

    boxPackStart mainTable resultNotebook PackGrow 3
    Graphics.UI.Gtk.set resultNotebook [notebookScrollable := True, notebookTabPos := PosTop]

    sep2 <- hSeparatorNew
    boxPackStart mainTable sep2  PackNatural 3

    quitBox <- hBoxNew False 10
    quitbutton <- buttonNewFromStock stockQuit
    disableButtonIfInBroadway quitbutton

    boxPackEnd quitBox quitbutton PackNatural 3

    boxPackStart mainTable quitBox  PackNatural 3

    let inputs = AnalysisParametersFields separatedGenerationsSwitch hardSelectionTresholdScale populationSizeScale optimumMovements

    _ <- on runButton buttonActivated (runSimulation nameField inputs resultNotebook window)
    _ <- on resetButton buttonActivated (resetOptions nameField inputs)
    _ <- on quitbutton buttonActivated mainQuit
    _ <- on window objectDestroy mainQuit

    placeAndSizeWindow window mainTable
    widgetShowAll window

    return window

main :: IO ()
main = do
    _ <- initGUI

    window <- prepareWindow

    mainGUI

extractParameters :: AnalysisParametersFields -> IO AnalysisParameters
extractParameters parameterFields = do
    separatedGen <- toggleButtonGetActive $ separatedGenerationsField parameterFields
    popSize <- spinButtonGetValueAsInt $ populationSizeField parameterFields
    hardSelectionTreshold <- spinButtonGetValue $ hardSelectionTresholdField parameterFields
    optimumMovements <- mapM optimumChangesGetValue $ optimumMovementFields parameterFields
    return AnalysisParameters {
          separatedGenerations = separatedGen,
          hardSelectionTreshold =  hardSelectionTreshold,
          populationSize =  popSize,
          optimumChange = optimumMovements,
          maxAge = 15,
          countOfBases = 20
          }
      where
        optimumChangesGetValue :: (SpinButton, SpinButton, SpinButton) -> IO(Double, Double, Double)
        optimumChangesGetValue (period, amplitude, gradient) = do
            period' <- spinButtonGetValue period
            amplitude' <- spinButtonGetValue amplitude
            gradient' <- spinButtonGetValue gradient
            return (period', amplitude', gradient')

presentationMode :: CursorType -> Window -> IO()
presentationMode cursorType window = do
      display <- fromJust <$> displayGetDefault
      waitCursor <- cursorNewForDisplay display cursorType
      drawWindow <- widgetGetWindow window
      drawWindowSetCursor (fromJust drawWindow) $ Just waitCursor

runSimulation :: Entry -> AnalysisParametersFields -> Notebook -> Window -> IO()
runSimulation nameField parameterFields resultNotebook window =
    do
        parameters <- extractParameters parameterFields
        _ <- presentationMode Watch window
        name <- entryGetText nameField
        _ <- forkIO $ do
            let simResults = computeSimulation parameters
            print simResults
            mapM_ (showResult resultNotebook name) simResults

            postGUIAsync $ do
                _ <- presentationMode LeftPtr window
                widgetShowAll window
                return ()
        return ()

resetOptions :: Entry -> AnalysisParametersFields -> IO()
resetOptions nameField parameterFields =
  do
    entrySetText nameField ""
    spinButtonSetValue (hardSelectionTresholdField parameterFields) 0.0
    spinButtonSetValue (populationSizeField parameterFields) 300
    toggleButtonSetActive (separatedGenerationsField parameterFields) False

    mapM_ resetOptimumMovement (optimumMovementFields parameterFields)
    return ()
        where resetOptimumMovement (per, ampl, a) = do
                spinButtonSetValue per 200
                spinButtonSetValue ampl 0.0
                spinButtonSetValue a 0.0
                return ()

closeTab :: Notebook -> Widget -> IO()
closeTab notebook widget =
  do
    page <- fromJust <$> notebookPageNum notebook widget
    notebookRemovePage notebook page
    return ()

showResult resultNotebook name (resultName, resultValue) = do
            let fileName = "/tmp/img_" ++ name ++ "_" ++ resultName ++ ".png"

            toFile def fileName $ do
                layout_title .= (name ++ "/" ++ resultName)
                layout_all_font_styles . font_size %= (*1.5)
                plot $ points resultName resultValue

            putStrLn $ "Ploted to " ++ fileName
            postGUIAsync $ do
                drawingArea <- imageNew
                imageSetFromFile drawingArea fileName
                tabLabel <- labelNew $ Just (name ++ "/" ++ resultName)
                tabClose <- buttonNew
                closeImage <- imageNewFromStock stockClose IconSizeMenu
                containerAdd tabClose closeImage
                on tabClose buttonActivated (closeTab resultNotebook $ castToWidget drawingArea)
                tabTop <- hBoxNew False 1
                boxPackEnd tabTop tabClose PackNatural 1
                boxPackEnd tabTop tabLabel PackNatural 1
                widgetShowAll tabTop
                tabMenu <- labelNew $ Just "xxxx"
                _ <- notebookPrependPageMenu resultNotebook drawingArea tabTop tabMenu
                removeFile fileName
                widgetShowAll resultNotebook
                _ <- notebookSetCurrentPage resultNotebook 0
                return ()
