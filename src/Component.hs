{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Component where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow


type Board = [[Text]]

data GameMode = None | Normal | Hard | PvP deriving (Eq, Show)

instance TextShow GameMode where
    showt :: GameMode -> Text
    showt None = " "
    showt Normal = "Normal mode: "
    showt Hard = "Hard mode: "
    showt PvP = "PvP mode: "

data Winner = Undecided | Tie | Xwin | Owin deriving (Eq, Show)

instance TextShow Winner where 
    showt :: Winner -> Text
    showt Undecided = "     "
    showt Tie = "Tie!"
    showt Xwin = "X wins!"
    showt Owin = "O wins!"

data AppModel = AppModel {
  _isXturn :: Bool,
  _board :: Board,
  _mode :: GameMode,
  _winner :: Winner,
  _gameRound :: Int
} deriving (Eq, Show)

-- data NewModel =  NewModel {
--   _newClick :: Int
-- } deriving (Eq,Show)

data AppEvent
  = AppInit
  | GameReset
  | ChooseMode GameMode
  | PlayMove (Int, Int)
  deriving (Eq, Show)

makeLenses 'AppModel



tileButton model (r,c) e = button (" " <> getTile (model ^. board) (r,c)) e `styleBasic` [width 80, height 80]


getTile :: [[Text]] -> (Int,Int) -> Text
getTile curBoard (row,col) = (curBoard!!row)!!col

emptyTile, xTile, oTile :: Text
-- xTile :: Text
-- oTile :: Text
emptyTile = "-"
xTile = "X"
oTile = "O"


initBoard :: [[Text]]
initBoard = 
    [
        [
            "-", "-", "-"
        ],

        [
            "-", "-", "-"
        ],

        [
            "-", "-", "-"
        ]

    ]


initModel :: AppModel
initModel = AppModel True initBoard None Undecided 0