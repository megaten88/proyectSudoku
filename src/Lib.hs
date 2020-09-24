module Lib
    ( someFunc
    ) where

import Interface

someFunc :: IO (Maybe BoardSudoku)
someFunc = getSudoku
