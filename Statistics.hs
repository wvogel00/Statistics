module Statistics where

import System.Random
import Data.List

type Pos = (Float,Float)

mkRands k = randomRs (0.0,1.0) $ mkStdGen k --乱数リスト

toStr i (x,y) = show x ++ "\t" ++ show (y/fromIntegral i) ++ "\n"

simulateProb :: (Int,Int) -> Int -> [(Float,Float)] -> String
simulateProb (_,0) _ _ = []
simulateProb (i,k) rank rs'
 = toStr i chi2++toStr i x2++simulateProb (i,k-1) rank (tail rs') where
    rs = take rank $ mkNormalRands rs' --正規乱数をn個取る
    fs = dividePhenom rank sig rs--n個に分けられた事象
    es = map ((/fromIntegral i).(fromIntegral.length)) fs    --期待値
    chi2 = actuallyChiSquare rank avg sig rs
    x2 = xSquare rank $ zip (map (fromIntegral.length) fs) es
    (avg,sig) = (0,1)
    rank' = fromIntegral rank

--分散と平均を計算
calcParam rank' rs = (avg, (sum $ map (\r -> r^2-avg^2) rs)/rank') where
    avg = sum rs/rank' 

--正規乱数リストを生成(box-muller)
mkNormalRands = map boxMuller where
    boxMuller (a,b) = sqrt (-2*log a) * sin (2*pi*b)

--sampleDataからX^2値と確率密度を求める
xSquare :: Int -> [(Float,Float)] -> Pos
xSquare rank xys = (x,y) where
    x = sum $ map (\(f,e) -> (f-e)^2/e) $ xys
    y = lowerDistribute rank x 

--χ二乗値、確率密度を式にそって計算
actuallyChiSquare :: Int -> Float -> Float -> [Float] -> (Float,Float)
actuallyChiSquare rank avg sig xs = (x,y) where
    x = sum $ map (\x -> (x-avg)^2/sig^2) xs
    y = lowerDistribute rank x

--自由度とカイ二乗値から確率密度を計算
probDensity :: Int -> Float -> Float
probDensity n x = x**(n'-1)*exp(-x/2)/2**n'/gammaF n' where
    n' = fromIntegral n/2
 --sqrt (1/2)/gammaF (fromIntegral k/2) * 1/sqrt x * exp(-x/2)

--ガンマ関数
gammaF :: Float -> Float
gammaF 0.5 = sqrt pi
gammaF 1 = 1
gammaF z = (z-1)*gammaF (z-1)

--下側累積確率(x,自由度nから)
lowerDistribute :: Int -> Float -> Float
lowerDistribute n x = sum $ take (floor $ 20*x) $ repeat f where
    n' = fromIntegral n/2
    f = x**(n'-1)*exp(-x/2)/2**n'/gammaF n'

upperDistribute :: Int -> Float -> Float -- 上側累積確率
upperDistribute n x = 1.0 - lowerDistribute n x

 --階級数と分散を受け、正規乱数をn個の事象（階級）に分ける
dividePhenom :: Int -> Float -> [Float] -> [[Float]]
dividePhenom rank sig = snd.unzip.foldl divide (zip ranks (repeat [])) where
    maxRange = 4*sig
    ranges = iterate f $ negate maxRange
    f = \x -> 2*maxRange / fromIntegral rank + x
    ranks = take rank $ zip ranges $ tail ranges
    divide ls r = if abs r > maxRange then ls else map (divide' r) ls
    divide' r d@(range,s) = if inRank range r then (range,r:s) else d

--値が事象に含まれるか否か
inRank (start,end) v = start <= v && v <= end

--階級数と分散を受け取り、各階級における度数と下側確率を計算
--calcFrecuency :: Int -> Float -> [Float] -> [Int]
--calcFrecuency rank sig = map length . dividePhenom rank sig

--検定
--優位水準と度数リストを受け取り、検定結果を返す
assay :: Float -> [Float] -> (Bool,Float)
assay a xs = (True,0)

--帰無仮説採択or棄却
isNorm a p = p > a
