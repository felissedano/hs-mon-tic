{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Game
import Component
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L
import Monomer.Core.Lens (HasFontColor(fontColor))



buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Tic Tac Toe Monomer",
      spacer `styleBasic` [paddingV 20],
      -- Main menu to select game mode
      hstack [
        label "Select Mode: ",
        spacer,
        button "Normal" $ ChooseMode Normal,
        spacer,
        button "Hard" $ ChooseMode Hard,
        spacer,
        button "PvP" $ ChooseMode PvP
      ],
      -- label $ "" <> showt (model ^. mode),
      label (showt (model ^. mode) <> "                   " <> showt (model ^. winner)) `styleBasic` [paddingV 20],
      -- The first row of the board
      hstack[
        spacer `styleBasic` [paddingH 40],
        tileButton model (0,0) (PlayMove (0,0)),
        tileButton model (0,1) (PlayMove (0,1)),
        tileButton model (0,2) (PlayMove (0,2))

      ],
      -- The second row of the board
      hstack [
        spacer `styleBasic` [paddingH 40],
        tileButton model (1,0) (PlayMove (1,0)),
        tileButton model (1,1) (PlayMove (1,1)),
        tileButton model (1,2) (PlayMove (1,2))
      ],
      -- The third row of the board
      hstack [
        spacer `styleBasic` [paddingH 40],
        tileButton model (2,0) (PlayMove (2,0)),
        tileButton model (2,1) (PlayMove (2,1)),
        tileButton model (2,2) (PlayMove (2,2))
      ],
      -- test row
      hstack [
        spacer `styleBasic` [paddingH 80],
        button "Reset"  GameReset,
        spacer,
        label $ "Winnner: " <> showt (model ^. winner)
      ]
    ] `styleBasic` [padding 10]


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  GameReset -> [Model initModel]
  ChooseMode gameMode -> [Model $ chooseMode initModel gameMode]
  PlayMove (r,c) -> [Model (placeTile (model ^. mode) (r,c) model)]--[Model (model & board .~ (placeTile (model ^. mode) (model ^. isXturn) (model ^. board) (r,c)))] --, Model (model & clickCount +~ 1)] -- (model ^. board & element t .~ "O"))]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  print $ model ^. board
  where
    config = [
      appWindowTitle "Tic-Tac-Toe-Monomer",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel True initBoard None Undecided 0