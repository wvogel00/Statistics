module Statistics where

import System.Random
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
mkNormalRands = map boxMuller

--Box-Muller法を用いてN(0,1)正規乱数を生成する
boxMuller :: (Float,Float) -> Float
boxMuller (a,b) = sqrt (-2*log a) * sin (2*pi*b)

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
 --階級数と分散を受け取る
 --その際、上下3σ内でカットした上で10rankに分割していく。
dividePhenom :: Int -> Float -> [Float] -> [[Float]]
dividePhenom n sig = snd.unzip.foldl divide (zip ranks $ repeat [0]) where
    maxRange = 3*sig
    ranges = iterate (+maxRange/(fromIntegral $ div n 2))  $ negate maxRange
    ranks = take n $ zip ranges $ tail ranges
    inRank (s,e) v = s <= v && v <= e
    divide ls r = if abs r > maxRange then ls else map (divide' r) ls
    divide' r d@(range,s) = if inRank range r then (range,r:s) else d

--階級数と分散を受け取り、各階級における度数と下側確率を計算
calcFrecuency :: Int -> Float -> [Float] -> [Int]
calcFrecuency n sig = map length . dividePhenom n sig

--検定
--優位水準と度数リストを受け取り、検定結果を返す
assay :: Float -> [Float] -> (Bool,Float)
assay a xs = (True,0) where
    n = fromIntegral $ length xs
    xs' = group $ sort xs :: [[Float]]
    _X = sum (map _Xf xs') / n :: Float
    _Xf :: [Float] -> Float
    _Xf ss = sum ss/fromIntegral (length ss) * fromIntegral (length ss)
    _V = (n*sum (map _Vf xs') - _X^2 ) / n^2
    _Vf :: [Float] -> Float
    _Vf ss = _Xf ss * sum ss/fromIntegral (length ss) :: Float
    _Zi ss =( sum ss/fromIntegral (length ss)  - _Xf ss) / sqrt (_Vf ss)

--帰無仮説採択or棄却
isNorm a p = p > a
