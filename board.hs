
import Data.List ((\\),transpose, elemIndex) 
import Text.Printf (printf)
import Network.HTTP.Client( Response(responseBody), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.ByteString.Lazy.Char8 (unpack)
import Text.XML.HXT.Core ( (>>>), runX )
import Text.HandsomeSoup ( (!), css, parseHtml )

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


-- | The side-length of the sudoku.
boardParameter :: Int
boardParameter = 9

-- | The side-length of boxes in the sudokus.
cellSize :: Int
cellSize = 3

-- | The symbols which can be inserted into sudokus.
dataValues :: [Char]
dataValues = "123456789"

-- | The symbol representing an empty field.
blankval :: Char
blankval = ' '




----------------- Creating data
newtype Sudoku = Sudoku String

instance Show Sudoku where
    -- | Turns the sudoku into a pretty string representation of a board.
    show (Sudoku s) = "\n" ++ (concat $ map showThird grouped) ++ line
        where grouped     = groupSize cellSize . map (groupSize cellSize) . groupSize boardParameter $ s
              showThird t = line ++ (concat $ map showRow t)
              showRow r   = (concat $ map showRowThird r) ++ "|\n"
              showRowThird rt = "|" ++ (concat $ map showCell rt)
              showCell c  = " " ++ (c : " ")
              line        = (concat $ replicate boardParameter "---") ++ (concat $ replicate cellSize "-") ++ "-\n"

-- Boolean, True if sudoku can be created
validatePossible :: String -> Bool
validatePossible dataString = (length dataString == (boardParameter * boardParameter)) &&
          (all (`elem` blankval:dataValues) dataString)

-- Creates a sudoku from a valid string.
createFrom :: String -> Maybe Sudoku
createFrom dataString
    | validatePossible dataString   = Just $ Sudoku dataString
    | otherwise = Nothing

-- | Returns the string representation of a sudoku.
toString :: Sudoku -> String
toString (Sudoku s) = s



-----------------
----------- get the random generated sudoku
getSudoku = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (dataurl 33)
    response <- httpLbs request manager
    let htmlDocument = parseHtml $ unpack $ responseBody response
    values <- runX $ htmlDocument >>> css "input.sgrid" ! "value"
    let dataCellValues = joinGroup . joinGroup . transpose . groupSize 3 . groupSize 9
                         . joinGroup . joinGroup . transpose . groupSize 3 . groupSize 3 $ values
    let sudokuString = concat $ map (\v -> if v == "" then blankval:"" else v) dataCellValues
    pure (createFrom sudokuString)
    