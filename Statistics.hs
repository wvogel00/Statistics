module Statistics where

import System.Random
import Data.Time.Clock
import Graphics.UI.WX

mkRands k= randomRs (0.0,1.0)$mkStdGen k--乱数リスト

timeToInt :: DiffTime -> Int
timeToInt = floor.read.init.show

--事象数を受け取り、算出したχ^2分布をファイルに書き出す
simulateF :: Window a -> Int -> IO()
simulateF form n = do
    t <- (getCurrentTime >>= return.utctDayTime)
--    let xs = mkRands $ timeToInt t
    file <- fileSaveDialog form True True "save data" [("data",["*.data"])] "" ""
    case file of
        Just filename -> writeFile filename ""
        Nothing       -> return()


