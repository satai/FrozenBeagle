{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
module Main where


import "gtk3" Graphics.UI.Gtk

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Concurrent

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main :: IO ()
main = do
    _ <- initGUI

    window  <- windowNew
    mainTable <- vBoxNew False 3

    settingsBox <- vBoxNew True 10

    settingsSwitchesBoxLeft <- vBoxNew False 0
    nameField <- entryNew
    separatedGenerationsSwitch <- checkButtonNewWithLabel "Separated Generations"
    multipleSimulationsSwitch <- checkButtonNewWithLabel "Multiple Simulations"
    populationSizeScale <- spinButtonNewWithRange  10 10000 10
    nameFieldBox <- hBoxNew False 10
    nameFieldlabel <- labelNew (Just "Description")
    boxPackStart nameFieldBox nameFieldlabel PackNatural 0
    boxPackStart nameFieldBox nameField PackNatural 10
    boxPackStart settingsSwitchesBoxLeft nameFieldBox PackNatural 0

    populationSizeBox <- hBoxNew False 10
    populationSizeLabel <- labelNew (Just "Population Size")

    boxPackStart populationSizeBox populationSizeLabel PackNatural 0
    boxPackStart populationSizeBox populationSizeScale PackNatural 10
    boxPackStart settingsSwitchesBoxLeft populationSizeBox PackNatural 0

    boxPackStart settingsSwitchesBoxLeft separatedGenerationsSwitch PackNatural 0
    boxPackStart settingsSwitchesBoxLeft multipleSimulationsSwitch PackNatural 0

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

    drawingArea <- imageNew

    _ <- on runButton buttonActivated (runSimulation nameField drawingArea)
    
    boxPackStart mainTable drawingArea  PackGrow 3

    sep2 <- hSeparatorNew
    boxPackStart mainTable sep2  PackNatural 3

    quitBox <- hBoxNew False 10
    quitbutton <- buttonNewFromStock stockQuit
    boxPackEnd quitBox quitbutton PackNatural 10

    boxPackStart mainTable quitBox  PackNatural 3

    _ <- on quitbutton buttonActivated mainQuit
    _ <- on window objectDestroy mainQuit

    Graphics.UI.Gtk.set window [ windowDefaultWidth := 1024,
                                 windowDefaultHeight := 768,
                                 containerBorderWidth := 10,
                                 containerChild := mainTable ]

    widgetShowAll window
    mainGUI


runSimulation :: Entry -> Image -> IO()
runSimulation nameField drawingArea =
    do
        name <- entryGetText nameField
        _ <- forkIO $ do
            toFile def ("/tmp/img_" ++ name ++ ".png") $ do
                layout_title .= name
                layout_all_font_styles . font_size %= (*1.5)
                plot (line "lines" [signal [0,(0.5)..400]])
                plot (points "points" (signal [0,7..400]))

            postGUIAsync $ do
                imageSetFromFile drawingArea ("/tmp/img_" ++ name ++ ".png")
                -- TODO delete
                return ()

        return ()