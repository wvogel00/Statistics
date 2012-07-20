module S2 where

import System.Random
import Data.List

type Pos = (Double,Double)

chiSquareGragh rank = map (\x -> (x , probDensity (rank-1) x)) [0,0.1..1000]

--N(0,1)の正規乱数を生成
mkNormalRands = map boxMuller
boxMuller (a,b) = sqrt (-2*log a) * sin (2*pi*b)

mkRands :: Int -> [Double]	--乱数リスト
mkRands k = randomRs (0,1) $ mkStdGen k

length' = fromIntegral.length

toStr (x,y) = show x ++ "\t" ++ show y ++ "\n"
(avg,sig) = (0,1)

--toPos :: Float -> Float -> Float -> Pos
toPos rank n x = (x,probDensity (rank-1) x)

simulate :: (Double,Int) -> Double -> [(Double,Double)] -> String
simulate (_,0) _ _ = []
simulate (n,k) rank rs'
 = dataStr ++ simulate (n,k-1) rank nextRs where
    rs = take (floor n) $ mkNormalRands rs' --N(0,1)のリスト
    nextRs = zip (mkRands k) $ mkRands (k*2-1) --次の乱数リスト
    dataStr = toStr (toPos rank n x2) --データをStringに変換
    x2 = xSquare rank n rs

--rank個の事象に分割
mkFrequency rank sig = map convert.foldl divide (zip ranks (repeat [])) where
    ranks = take (floor $rank+0.5) $ zip ranks' $ tail ranks'
    ranks' = iterate (\x -> 6*sig/rank + x) (-3*sig)
    convert (ranks,fs) = (ranks,length' fs)

--期待値のリスト
mkExpected rank n = (\(s,e) -> n*(lowerDensity e - lowerDensity s))

--X^2値を計算
xSquare rank n rs = (+negate n).sum.map (\(f,e) -> f^2/e) $ zip fs es where
    (ranks,fs) = unzip $ mkFrequency rank sig rs
    es = map (mkExpected rank n) ranks

--事象の分割,数え上げ
divide ls r = if abs r > 4*sig then ls else map (count r) ls
count v d@((s,e),ks) = if s <= v && v <= e then (fst d,v:ks) else d

--下側累積確率
lowerDensity x = (\(_,p,_)-> p+0.5) $ foldl f (t1,t1,t1) [3,5..200] where
    f (t,p,prev) i = (t*x^2/i,p+t*x^2/i,p)
    t1 = x*exp (-x^2/2) / sqrt (2*pi)

probDensity k x = x**(k/2-1)*exp(-x/2)/2**(k/2) / gamma (k/2)

--ガンマ関数
gamma 0.5 = sqrt pi
gamma 1 = 1
gamma z = (z-1)*gamma (z-1)
