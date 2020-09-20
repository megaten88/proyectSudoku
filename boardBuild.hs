import Data.List ((\\))

-- Checks if there is already a number on the list
checkDouble :: Eq a =>[a] -> Bool
checkDouble [] = True
checkDouble(head:tail) = head `notElem` tail && checkDouble tail

--- Checking if the list has only one element
hasOnlyOne:: [x] -> Bool
hasOnlyOne [x] = True
hasOnlyOne _ = False

