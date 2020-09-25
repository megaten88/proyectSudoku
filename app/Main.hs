module Main where
import           Solver
import           Api
import           Interface
import           Data.GI.Base
import qualified GI.Gtk        as Gtk


-- | Funcion principal abre una ventana  GTK  en la que se desarrolla el juego


main :: IO ()
main = do        
        Gtk.init Nothing
        ui <-  buildBoardSudokuUI
        cellsBindHandlers (cells ui) (popover ui)
        numbersBindHandlers (numberButtons ui) (popover ui)
        on (clearButton ui) #clicked $ writePopoverRelativeCell (popover ui) $ defaultValue
        on (newButton ui) #clicked $ newGame (cells ui) (getSudoku "func" False)
        on (checkButton ui) #clicked $ checkBoard (cells ui) 
        #showAll (window ui)
        Gtk.main
