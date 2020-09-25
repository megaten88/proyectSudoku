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

-- | Longitud lateral de las cajas (9 cajas).
boxsize :: Int
boxsize = 3

-- | Simbolos que se pueden insertar.
valuesCells :: [Char]
valuesCells = "123456789"

-- | Verdadero si a partir de una cadena determinada se puede crear un juego.
validate :: String -> Bool
validate s = (length s == (boardsize * boardsize)) &&
          (all (`elem` defaultValue:valuesCells) s)

-- | Crea un juego a partir de una cadena.
createString :: String -> Maybe BoardSudoku
createString s
    | validate s   = Just $ BoardSudoku s
    | otherwise = Nothing

-- | retorna el string que representa BoardSudoku.
toString :: BoardSudoku -> String
toString (BoardSudoku s) = s




data BuilderCastException = UnknownIdException String deriving (Show, Typeable)

instance Exception BuilderCastException

-- administración de funcion cell
type Cell = Button
type Cells = [Cell]

newtype BoardSudoku = BoardSudoku String

fromString :: [Char] -> String
fromString [c] = [c]

------------- Construccion de interfaz grafica
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

-- | Los identificadores de las celdas del juego en el ui.
cellNames :: [Textual.Text]
cellNames = map (Textual.pack . (++) "cell") $ map show [1..81]

-- | Los identificadores de las entradas para los enteros en el ui
numberNames :: [Textual.Text]
numberNames = map (Textual.pack . (++) "number") $ map show [1..9]

-- | Toma un builder y devuelve el objeto con un nombre dado
builderGetTyped :: (IsBuilder a, GObject o, MonadIO m) => a -> Textual.Text -> (ManagedPtr o -> o) -> m o
builderGetTyped builder ident gtype =
    liftIO $ do
        o <- builderGetObject builder ident
        case o of
            Just a  -> unsafeCastTo gtype a
            Nothing -> throw $ UnknownIdException $ Textual.unpack ident

-- | Al igual que builderGetTyped para una lista de nombres.
builderGetsTyped :: (GObject a, IsBuilder b, MonadIO m) => b -> [Textual.Text] -> (ManagedPtr a -> a) -> m [a]
builderGetsTyped b is t = sequence $ map (\i -> builderGetTyped b i t) is

-- | Crea la ventana principal de la aplicación a partir de un archivo de xml para el que se define la ruta
buildMainWindow :: MonadIO m => Textual.Text -> Textual.Text -> m (Window, Builder)
buildMainWindow name path = liftIO $ do
    builder <- builderNewFromFile path
    window  <- builderGetTyped builder name Window
    on window #destroy mainQuit
    cssFile <- Textual.pack <$> getDataFileName "ui/board.css"
    windowAddCss window cssFile
    pure (window, builder)

-- | Agrega a una ventana determinada un archivo css para el que se define la ruta.
windowAddCss :: (MonadIO m, IsWindow a) => a -> Textual.Text -> m ()
windowAddCss window path = liftIO $ do
    screen <- windowGetScreen window
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider path
    styleContextAddProviderForScreen screen cssProvider 1000

-- | Escribe un char en una celda de juego..
writeCell :: Cell -> Char -> IO ()
writeCell cell char = #setLabel cell (Textual.singleton char)

-- | Escribe un carácter en una celda que está asociada a un popover dado
--   el popover se cierra después.
writePopoverRelativeCell :: Popover -> Char -> IO ()
writePopoverRelativeCell popover char = do
    widget <- #getRelativeTo popover
    cell   <- unsafeCastTo Button widget
    writeCell cell char
    #hide popover

-- | Vincula los cells a los botones.
cellsBindHandlers :: Cells -> Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent  $ focusInHandler c
        ) cells
    where focusInHandler c _ = do cellShowPopover c popover; pure False


charToString :: Char -> String
charToString c = [c]


-- | Comprueba y devuelve si una celda determinada contiene el valor correcto.
--   Si el valor no es correcto, la celda se marca
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

-- | Comprueba si todas las celdas dadas contienen el valor correcto.
--   Marca visualmente las celdas correctas o incorrectas.
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

-- | Asocia el popover a un botón dado y lo muestra.
cellShowPopover :: Cell -> Popover -> IO ()
cellShowPopover cell popover = do
    popover `set` [#relativeTo := cell]
    #show popover


-- | Vincula los handlers a una lista de botones numéricos.
numbersBindHandlers :: [Button] -> Popover -> IO ()
numbersBindHandlers buttons popover = mapM_ (\b -> do
            on b #clicked $ numberButtonInsert b popover
        ) buttons

-- | Inserta el contenido de un botón en una celda asociada al popover.
numberButtonInsert :: Button -> Popover -> IO ()
numberButtonInsert button popover = do
    label <- #getLabel button
    writePopoverRelativeCell popover $ Textual.head label

-- | Escribe un juego en una lista de botones.
writeSudoku :: Cells -> String -> IO ()
writeSudoku cells game = do
    let sudokuChars = game
    sequence_ $ zipWith (\c sc -> do
            writeCell c sc
            if sc == defaultValue
                then c `set` [#sensitive := True]
                else c `set` [#sensitive := False]
        ) cells sudokuChars

-- | Almacena una solución dada en los nombres de las celdas pasadas
writeSolution :: Cells -> String -> IO ()
writeSolution cells game = do
    let sudokuChars = game
    sequence_ $ zipWith (\c sc -> do
            #setName c (Textual.singleton sc)
        ) cells sudokuChars

-- | Prepara un nuevo juego en  la ui.
newGame  :: Cells -> IO(String) ->  IO ()
newGame cells gameString = do
    game <- gameString
    let solve = execute game
    putStrLn solve
    let solveString = (filter (/= ' ') solve)
    writeSudoku cells game
    writeSolution cells (filter (/= '\n') solveString)