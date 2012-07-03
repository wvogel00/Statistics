module Statistics where

import System.Random
import Graphics.UI.WX

mkRands k = randomRs (0.0,1.0) $ mkStdGen k --乱数リスト

simulateProb :: Int -> [Double] ->[Double] -> String
simulateProb n rs1 rs2 = "hello,world"
 -- = mkNormalRands $ zip rs1 rs2

--正規乱数リストを生成
mkNormalRands :: [(Double,Double)] -> [Double]
mkNormalRands = foldl mkNormalRands' []

mkNormalRands' xs r = nr1:nr2:xs where
    (nr1,nr2) = boxMuller r

--Box-Muller法を用いてN(0,1)正規乱数を生成する
boxMuller :: (Double,Double) -> (Double,Double)
boxMuller (a,b) = (sqrt a' * sin b' , sqrt a'*sin b') where
    (a',b') = (-2*log a , 2*pi*b)
