module Interface(
    loadSudoku
    , BoardSudoku
) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.GI.Base
import qualified Data.Text              as Textual
import           Data.Typeable
import           GI.Gtk
import           Paths_sudoku
import           Data.List ((\\),transpose, elemIndex) 
import           Text.Printf (printf)
import           Network.HTTP.Client( Response(responseBody), httpLbs, newManager, parseRequest)
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Data.ByteString.Lazy.Char8 (unpack)
import           Text.HandsomeSoup
import           Text.Printf                (printf)
import           Text.XML.HXT.Core

-- Checks if there is already a number on the list
checkDouble :: Eq x =>[x] -> Bool
checkDouble [] = True
checkDouble(head:tail) = head `notElem` tail && checkDouble tail

--- Checking if the list has only one element
hasOnlyOne:: [x] -> Bool
hasOnlyOne [x] = True
hasOnlyOne _ = False

--- Group a tail list by defined size
groupSize :: Int -> [x] ->[[x]]
groupSize size [] = []
groupSize size tail = (take size tail) : groupSize size (drop size tail)

-- Used for joining groups or concatenate them
joinGroup :: [[b]] -> [b]
joinGroup = concat

delete :: Eq a => [a] -> [a] -> [a]
delete = flip (\\)

-- Get random sudoku board
dataurl :: Int ->  String
dataurl value = "https://kjell.haxx.se/sudoku/?visade=" ++ (show $ value) ++ "&seed=%28random+seed%29&action=Create+a+field&hardchange=0"

---
blankval :: Char
blankval = '.'

boardsize :: Int
boardsize = 9

-- | The side-length of boxes in the sudokus.
boxsize :: Int
boxsize = 3

-- | The symbols which can be inserted into sudokus.
cellvals :: [Char]
cellvals = "123456789"

-- | True iff from a given string a sudoku can be created.
valid :: String -> Bool
valid s = (length s == (boardsize * boardsize)) &&
          (all (`elem` blankval:cellvals) s)

-- | Creates a sudoku from a valid string.
fromString :: String -> Maybe BoardSudoku
fromString s
    | valid s   = Just $ BoardSudoku s
    | otherwise = Nothing

-- | Returns the string representation of a BoardSudoku.
toString :: BoardSudoku -> String
toString (BoardSudoku s) = s


data BuilderCastException = UnknownIdException String deriving (Show, Typeable)

instance Exception BuilderCastException

-- Cell Management
type Cell = Button
type Cells = [Cell]

newtype BoardSudoku = BoardSudoku String


loadSudoku :: IO (Maybe BoardSudoku)

loadSudoku = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (dataurl 33)
    response <- httpLbs request manager
    let doc = parseHtml $ unpack $ responseBody response
    values <- runX $ doc >>> css "input.sgrid" ! "value"
    -- in the html the values are aranged block wise and not row wise
    let transposedValues = joinGroup . joinGroup . transpose . groupSize 3 . groupSize 9
                         . joinGroup . joinGroup . transpose . groupSize 3 . groupSize 3 $ values
    let sudokuString = concat $ map (\v -> if v == "" then blankval:"" else v) transposedValues
    putStrLn sudokuString
    pure (fromString sudokuString)


