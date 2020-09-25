module Interface(
    BoardSudoku
    , BuilderCastException(..)
    , BoardSudokuUI
    , window
    , cells
    , popover
    , numberButtons
    , clearButton
    , newButton
    , checkButton
    , writePopoverRelativeCell
    , cellsBindHandlers
    , numbersBindHandlers
    , buildBoardSudokuUI
    , newGame
    , checkBoard
) where


import           Control.Concurrent     (forkIO, threadDelay)
import Control.Exception ( Exception, throw )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.GI.Base
    ( set, unsafeCastTo, on, AttrOp((:=)), GObject, ManagedPtr )
import qualified Data.Text              as Textual
import Data.Typeable ( Typeable )
import GI.Gtk
    ( set,
      unsafeCastTo,
      on,
      AttrOp((:=)),
      GObject,
      ManagedPtr,
      mainQuit,
      builderGetObject,
      builderNewFromFile,
      cssProviderLoadFromPath,
      cssProviderNew,
      styleContextAddProviderForScreen,
      toWidget,
      windowGetScreen,
      Builder,
      IsBuilder,
      Button(..),
      CssProvider(CssProvider),
      Popover(..),
      IsWindow,
      Window(..) )
import           Paths_sudoku ( getDataFileName )
import           Data.List ((\\),transpose, elemIndex) 
import           Network.HTTP.Client( Response(responseBody), httpLbs, newManager, parseRequest)
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Data.ByteString.Lazy.Char8 (unpack)
import           Text.HandsomeSoup ()
import           Text.Printf                (printf)
import           Text.XML.HXT.Core ()
import           Data.Maybe                 (fromMaybe)
import           Solver (execute)

defaultValue :: Char
defaultValue = '.'

boardsize :: Int
boardsize = 9

-- | The side-length of boxes in the sudokus.
boxsize :: Int
boxsize = 3

-- | The symbols which can be inserted into sudokus.
valuesCells :: [Char]
valuesCells = "123456789"

-- | True iff from a given string a game can be created.
validate :: String -> Bool
validate s = (length s == (boardsize * boardsize)) &&
          (all (`elem` defaultValue:valuesCells) s)

-- | Creates a game from a validate string.
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

-- | The ids of the game cells in the ui file.
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

-- | Writes a character into a game cell.
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

-- | Binds the signal handlers to buttons.
cellsBindHandlers :: Cells -> Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent  $ focusInHandler c
        ) cells
    where focusInHandler c _ = do cellShowPopover c popover; pure False


charToString :: Char -> String
charToString c = [c]


-- | Checks and returns if a given cell contains the correct value.
--   If the value is not correct the cell gets visually marked.
checkByCell :: Cell -> IO Bool
checkByCell cell = do
    getSolution <- Textual.head <$> (toWidget cell >>= #getName)
    -- putStrLn $ "Solution = " ++ (charToString getSolution)
    actual <- Textual.head <$> #getLabel cell
    --putStrLn $ charToString actual
    let isCorrect = actual == getSolution
    style <- #getStyleContext cell
    if not isCorrect
        then #addClass style "incorrect"
        else #addClass style "correct"
    forkIO $ threadDelay 800000 >> #removeClass style "incorrect"
    pure isCorrect

-- | Checks if all given cells contain the correct value.
--   Visually marks the correct or incorrect cells.
checkBoard :: Cells -> IO ()
checkBoard cells = do
    allAreCorrect <- and <$> mapM checkByCell cells
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

-- | Writes a game into a list of buttons.
writeSudoku :: Cells -> String -> IO ()
writeSudoku cells game = do
    let sudokuChars = game
    sequence_ $ zipWith (\c sc -> do
            writeCell c sc
            if sc == defaultValue
                then c `set` [#sensitive := True]
                else c `set` [#sensitive := False]
        ) cells sudokuChars

-- | Stores a given solution in the names of the passed cells.
writeSolution :: Cells -> String -> IO ()
writeSolution cells game = do
    let sudokuChars = game
    sequence_ $ zipWith (\c sc -> do
            #setName c (Textual.singleton sc)
        ) cells sudokuChars

-- | Prepares a new game in the UI.
newGame  :: Cells -> IO(String) ->  IO ()
newGame cells gameString = do
    game <- gameString
    let solve = execute game
    putStrLn solve
    let solveString = (filter (/= ' ') solve)
    writeSudoku cells game
    writeSolution cells (filter (/= '\n') solveString)