module Main where
import           Solver
import           Api
import           Interface
import           Data.GI.Base
import qualified GI.Gtk        as Gtk


-- | The main function which opens a new GTK window in which sudoku games can be
-- played.


main :: IO ()
main = do
    --- For some reason if it is not called before init, it doesn't work         
        Gtk.init Nothing
        ui <-  buildBoardSudokuUI
        cellsBindHandlers (cells ui) (popover ui)
        numbersBindHandlers (numberButtons ui) (popover ui)
        on (clearButton ui) #clicked $ writePopoverRelativeCell (popover ui) $ defaultValue
        on (newButton ui) #clicked $ newGame (cells ui) (getSudoku "func" False)
        on (checkButton ui) #clicked $ checkBoard (cells ui) 
        #showAll (window ui)
        Gtk.main
