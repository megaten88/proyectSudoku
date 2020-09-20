
import Data.List ((\\),transpose, elemIndex) 
import Text.Printf (printf)
import Network.HTTP.Client( Response(responseBody), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.ByteString.Lazy.Char8 (unpack)

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