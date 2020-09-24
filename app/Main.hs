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
       
        func  <- getSudoku "func" False
        func1  <- getSudoku func True
        putStr $ execute func
        putStrLn ""
        putStr $ execute func1 
        
        Gtk.init Nothing
        ui <-  buildBoardSudokuUI
        cellsBindHandlers (cells ui) (popover ui)
        numbersBindHandlers (numberButtons ui) (popover ui)
        on (clearButton ui) #clicked $ writePopoverRelativeCell (popover ui) $ blankval
        on (newButton ui) #clicked $ newGame (cells ui) (func)
        on (checkButton ui) #clicked $ checkAll (cells ui) (func)
        #showAll (window ui)
        Gtk.main
