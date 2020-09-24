module Api(
    defaultValue
    , groupSize
    , getSudoku
) where

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (elemIndex, transpose)
import           Data.Maybe                 (fromMaybe)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.HandsomeSoup
import           Text.Printf                (printf)
import           Text.XML.HXT.Core

joinGroup :: [[b]] -> [b]
joinGroup = concat

defaultValue :: Char
defaultValue = '.'

-- Get random sudoku board
dataurl :: Int ->  String
dataurl value = "https://kjell.haxx.se/sudoku/?visade=" ++ (show $ value) ++ "&seed=%28random+seed%29&action=Create+a+field&hardchange=0"

--- Group a tail list by defined size
groupSize :: Int -> [x] ->[[x]]
groupSize size [] = []
groupSize size tail = (take size tail) : groupSize size (drop size tail)

getSudoku :: String-> Bool->IO (String)
getSudoku s b =  do
    if b==True
        then pure(s)
    else do
        manager <- newManager tlsManagerSettings
        request <- parseRequest (dataurl 33)
        response <- httpLbs request manager
        let htmlParse = parseHtml $ unpack $ responseBody response
        values <- runX $ htmlParse >>> css "input.sgrid" ! "value"
        -- in the html the values are aranged block wise and not row wise
        let transposedValues = joinGroup . joinGroup . transpose . groupSize 3 . groupSize 9
                            . joinGroup . joinGroup . transpose . groupSize 3 . groupSize 3 $ values
        let sudokuString = concat $ map (\v -> if v == "" then defaultValue:"" else v) transposedValues
        pure (sudokuString)

