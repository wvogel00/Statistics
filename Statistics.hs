module Statistics where

import System.Random
import Graphics.UI.WX

mkRands k = randomRs (0.0,1.0) $ mkStdGen k --乱数リスト

simulateProb :: Int -> [Float] ->[Float] -> String
simulateProb n rs1 rs2 = "hello,world"
 -- = take n $ mkNormalRands $ zip rs1 rs2

--座標データをファイルに書き出す時
format :: [(Float,Float)] -> String
format = concat . map (\(x,y) -> show x ++ "\t" ++ show y ++ "\n")

--ファイルデータを座標データに変換するとき
unFormat :: String -> [(Float,Float)]
unFormat = map ((\[x,y] -> (read x,read y)) . words) . lines

--正規乱数リストを生成
mkNormalRands :: [(Float,Float)] -> [Float]
mkNormalRands = foldl mkNormalRands' []
mkNormalRands' xs r = nr1:nr2:xs where
    (nr1,nr2) = boxMuller r

--Box-Muller法を用いてN(0,1)正規乱数を生成する
boxMuller :: (Float,Float) -> (Float,Float)
boxMuller (a,b) = (sqrt a' * sin b' , sqrt a'*sin b') where
    (a',b') = (-2*log a , 2*pi*b)

actuallyChiSquare :: Int -> [(Float,Float)]
actuallyChiSquare n = take n $ zip xs ys where
 xs = [-1.0,-0.999..1.0]
 ys = repeat 1.0
