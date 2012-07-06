module Statistics where

import System.Random
import Graphics.UI.WX
import Data.List

--乱数リスト
mkRands k = randomRs (0.0,1.0) $ mkStdGen k

simulateProb :: Int -> [Float] ->[Float] -> String
simulateProb n rs1 rs2 = "hello,world" where
 xs = take n $ mkNormalRands $ zip rs1 rs2 --正規乱数をn個取る

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

--χ二乗値、確率密度を式にそって計算
actuallyChiSquare :: Int -> [Float] -> (Float,Float)
actuallyChiSquare n rs = (x,y) where
    x = chiFunc $ take n rs
    y = probDensity 2 x

--N(0,1)のχ二乗値を計算
chiFunc :: [Float] -> Float
chiFunc = sum . map (^2)

--自由度とカイ二乗値から確率密度を計算
probDensity :: Int -> Float -> Float
probDensity k x = sqrt (1/2)/gammaF (fromIntegral k/2) * 1/sqrt x * exp(-x/2)

--ガンマ関数
gammaF :: Float -> Float
gammaF z = sum $ map (\t -> 1/sqrt z*exp (-t)) [0.5,1..1000]

 --正規乱数を事象に分ける
 --その際、上下3σ内でカットした上で10rankに分割していく。
dividePhenom :: Float -> [Float] -> [[Float]]
dividePhenom sig = snd.unzip.foldl divide (zip ranks $ repeat [0]) where
    maxRange = 3*sig
    ranges = iterate (+maxRange/5)  $ negate maxRange
    ranks = take 10 $ zip ranges $ tail ranges
    inRank (s,e) v = s <= v && v <= e
    divide ls r = if abs r > 3*sig then ls else map (divide' r) ls
    divide' r d@(range,s) = if inRank range r then (range,r:s) else d
