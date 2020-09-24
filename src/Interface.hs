module Interface(
    getSudoku
    , BoardSudoku
    , BuilderCastException(..)
    , BoardSudokuUI
    , window
    , cells
    , popover
    , numberButtons
    , clearButton
    , newButton
    , checkButton
    , getSudoku
    , writePopoverRelativeCell
    , solvePopoverRelativeCell
    , cellsBindHandlers
    , numbersBindHandlers
    , blankval
    , buildBoardSudokuUI
    , newGame
    , checkAll
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
import           Network.HTTP.Client( Response(responseBody), httpLbs, newManager, parseRequest)
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Data.ByteString.Lazy.Char8 (unpack)
import           Text.HandsomeSoup
import           Text.Printf                (printf)
import           Text.XML.HXT.Core
import           Data.Maybe                 (fromMaybe)

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
validate :: String -> Bool
validate s = (length s == (boardsize * boardsize)) &&
          (all (`elem` blankval:cellvals) s)

-- | Creates a sudoku from a validate string.
createString :: String -> Maybe BoardSudoku
createString s
    | validate s   = Just $ BoardSudoku s
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

fromString :: [Char] -> String
fromString [c] = [c]


getSudoku :: IO (String)
getSudoku = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (dataurl 33)
    response <- httpLbs request manager
    let htmlParse = parseHtml $ unpack $ responseBody response
    values <- runX $ htmlParse >>> css "input.sgrid" ! "value"
    -- in the html the values are aranged block wise and not row wise
    let transposedValues = joinGroup . joinGroup . transpose . groupSize 3 . groupSize 9
                         . joinGroup . joinGroup . transpose . groupSize 3 . groupSize 3 $ values
    let sudokuString = concat $ map (\v -> if v == "" then blankval:"" else v) transposedValues
    pure (sudokuString)



------------- BUILDING THE UI 
data BoardSudokuUI = BoardSudokuUI { window        :: Window
                         , cells         :: Cells
                         , popover       :: Popover
                         , numberButtons :: [Button]
                         , clearButton    :: Button
                         , newButton   :: Button
                         , checkButton   :: Button
                         }
buildBoardSudokuUI :: IO BoardSudokuUI
buildBoardSudokuUI = do
    uiFile            <- Textual.pack <$> getDataFileName "ui/board.ui"
    (window, builder) <- buildMainWindow "main" uiFile
    cells             <- builderGetsTyped builder cellNames Button
    popover           <- builderGetTyped builder "numberPopOver" Popover
    numberButtons     <- builderGetsTyped builder numberNames Button
    clearButton        <- builderGetTyped builder "clearButton" Button
    newButton       <- builderGetTyped builder "newButton" Button
    checkButton       <- builderGetTyped builder "checkButton" Button
    pure $ BoardSudokuUI window cells popover numberButtons clearButton newButton checkButton

-- | The ids of the sudoku cells in the ui file.
cellNames :: [Textual.Text]
cellNames = map (Textual.pack . (++) "cell") $ map show [1..81]

-- | The ids of the inputs for the numbers in the ui file.
numberNames :: [Textual.Text]
numberNames = map (Textual.pack . (++) "number") $ map show [1..9]

-- | Takes a builder and returns the object with a given name
--   typed as a given gtype.
builderGetTyped :: (IsBuilder a, GObject o, MonadIO m) => a -> Textual.Text -> (ManagedPtr o -> o) -> m o
builderGetTyped builder ident gtype =
    liftIO $ do
        o <- builderGetObject builder ident
        case o of
            Just a  -> unsafeCastTo gtype a
            Nothing -> throw $ UnknownIdException $ Textual.unpack ident

-- | Same as builderGetTyped for a list of names.
builderGetsTyped :: (GObject a, IsBuilder b, MonadIO m) => b -> [Textual.Text] -> (ManagedPtr a -> a) -> m [a]
builderGetsTyped b is t = sequence $ map (\i -> builderGetTyped b i t) is

-- | Builds the main application window from a xml definition file for which the
--   path is given.
buildMainWindow :: MonadIO m => Textual.Text -> Textual.Text -> m (Window, Builder)
buildMainWindow name path = liftIO $ do
    builder <- builderNewFromFile path
    window  <- builderGetTyped builder name Window
    on window #destroy mainQuit
    cssFile <- Textual.pack <$> getDataFileName "ui/board.css"
    windowAddCss window cssFile
    pure (window, builder)

-- | Adds to a given window a css file for which the path is given.
windowAddCss :: (MonadIO m, IsWindow a) => a -> Textual.Text -> m ()
windowAddCss window path = liftIO $ do
    screen <- windowGetScreen window
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider path
    styleContextAddProviderForScreen screen cssProvider 1000

-- | Writes a character into a sudoku cell.
writeCell :: Cell -> Char -> IO ()
writeCell cell char = #setLabel cell (Textual.singleton char)

-- | Writes a charachter into a cell which is associated to a given popover
--   The popover gets closed afterwards.
writePopoverRelativeCell :: Popover -> Char -> IO ()
writePopoverRelativeCell popover char = do
    widget <- #getRelativeTo popover
    cell   <- unsafeCastTo Button widget
    writeCell cell char
    #hide popover

-- -- | Solves a given cell.
-- solveCell :: Cell -> IO ()
-- solveCell cell = do
--     char <- Textual.head <$> #getName cell
--     writeCell cell char

-- -- | Solves all given cells.
-- solveAll :: Cells -> IO ()
-- solveAll = mapM_ solveCell

-- -- | Solves the cell currently relative to the popover.
solvePopoverRelativeCell :: Popover -> IO ()
solvePopoverRelativeCell popover = do
    cell <- #getRelativeTo popover >>= unsafeCastTo Button
    #hide popover

-- | Binds the signal handlers to buttons.
cellsBindHandlers :: Cells -> Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent  $ focusInHandler c
        ) cells
    where focusInHandler c _ = do cellShowPopover c popover; pure False

-- | Checks and returns if a given cell contains the correct value.
--   If the value is not correct the cell gets visually marked.
checkCell :: Cell -> IO Bool
checkCell cell = do
    solution <- Textual.head <$> (toWidget cell >>= #getName)
    actual <- Textual.head <$> #getLabel cell
    let isCorrect = actual == solution
    style <- #getStyleContext cell
    if not isCorrect
        then #addClass style "incorrect"
        else pure ()
    forkIO $ threadDelay 800000 >> #removeClass style "incorrect"
    pure isCorrect

-- | Checks if all given cells contain the correct value.
--   Visually marks the correct or incorrect cells.
checkAll :: Cells -> IO ()
checkAll cells = do
    allAreCorrect <- and <$> mapM checkCell cells
    if allAreCorrect
        then mapM_ (\cell -> do
            style <- #getStyleContext cell
            #addClass style "correct"
            forkIO $ threadDelay 800000 >> #removeClass style "correct"
        ) cells
        else pure ()

-- | Associates the popover to a given button and shows the popover.
cellShowPopover :: Cell -> Popover -> IO ()
cellShowPopover cell popover = do
    popover `set` [#relativeTo := cell]
    #show popover

-- | Binds the signal handlers to a list of number buttons.
numbersBindHandlers :: [Button] -> Popover -> IO ()
numbersBindHandlers buttons popover = mapM_ (\b -> do
            on b #clicked $ numberButtonInsert b popover
        ) buttons

-- | Inserts the content of a number button to a cell associated to the popover.
numberButtonInsert :: Button -> Popover -> IO ()
numberButtonInsert button popover = do
    label <- #getLabel button
    writePopoverRelativeCell popover $ Textual.head label

-- | Writes a sudoku into a list of buttons.
writeSudoku :: Cells -> String -> IO ()
writeSudoku cells sudoku = do
    let sudokuChars = sudoku
    sequence_ $ zipWith (\c sc -> do
            writeCell c sc
            if sc == blankval
                then c `set` [#sensitive := True]
                else c `set` [#sensitive := False]
        ) cells sudokuChars

-- | Stores a given solution in the names of the passed cells.
-- writeSolution :: Cells -> Sudoku -> IO ()
-- writeSolution cells sudoku = do
--     let sudokuChars = toString sudoku
--     sequence_ $ zipWith (\c sc -> do
--             #setName c (Textual.singleton sc)
--         ) cells sudokuChars

-- | Prepares a new game in the UI.
newGame  :: Cells -> IO(String) ->  IO ()
newGame cells gameString = do
    sudoku <- gameString
    -- let Just solution = head <$> solveSudoku sudoku
    writeSudoku cells sudoku
    -- writeSolution cells solution
