{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Component
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L
import Monomer.Core.Lens (HasFontColor(fontColor))


chooseMode :: AppModel -> GameMode -> AppModel
chooseMode model gameMode = case gameMode of
    PvP -> model & mode .~ gameMode
    _ -> model -- TODO normal mode and hard mode


-- checks if the place is empty if yes then place the tile, change the board depending on gamemode and check if there is a winner
placeTile :: GameMode -> (Int, Int) -> AppModel -> AppModel
placeTile gameMode (row,col) model = if isEmpty then case gameMode of
    None -> model --checkWinner (row,col) (model ^. isXturn) $ pvpMove model curBoard (row,col)
    Normal -> model -- normalMove mode isXturn model 
    Hard -> model
    PvP -> checkWinner (model ^. isXturn) $ pvpMove model curBoard (row,col)
    else model
    where
        curBoard = model ^. board
        selectedTile = (curBoard!!row)!!col
        isEmpty = selectedTile == emptyTile


checkWinner :: Bool -> AppModel -> AppModel
checkWinner xturn model
  |
       any (\row -> all (\col -> (curBoard!!row)!!col == curTile)[0..2]) [0..2] -- checks each rows
    || any (\col -> all (\row -> (curBoard!!row)!!col == curTile)[0..2]) [0..2] -- check each colums
    || all (\rowcol -> (curBoard!!rowcol)!!rowcol == curTile) [0..2] -- check diagnol from top left
    || all (\rowcol -> (curBoard!!rowcol)!!(2 - rowcol) == curTile) [0..2] = gameOver model curPlayer
  | curRound == 8 = gameOver model Tie
  | otherwise = (gameRound +~ 1) model
  where
      curBoard = model ^. board
      curTile = if xturn then xTile else oTile
      curPlayer = if xturn then Xwin else Owin
      curRound = model ^. gameRound


changeTurn :: AppModel -> AppModel
changeTurn model = model & isXturn .~ not (model ^. isXturn)

pvpMove :: AppModel -> [[Text]] -> (Int,Int) -> AppModel
pvpMove model curBoard (row, col) = changeTurn model' where
    curRow      = curBoard!!row
    curTile     = curRow!!col
    xTurn       = model ^. isXturn
    tile        = if xTurn then "X" else "O"
    newBoard    = curBoard & element row .~ (curRow & element col .~ tile)
    model'      = model & board .~ newBoard



-- normalMove :: AppModel -> [[Text]] -> (Int,Int) -> AppModel
-- normalMove model curBoard (row,col) = fail "Implement normal difficulty AI move"

gameOver :: AppModel -> Winner -> AppModel
gameOver model player =
    let model' = model & winner .~ player
                       & mode .~ None
    in model'
