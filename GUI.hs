import System.IO
import Graphics.Gnuplot.Simple
import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import Data.Time.Clock
import Control.Monad
import ReadSource
--import Statistics
import S2

main = start mainGUI

--UI
mainGUI :: IO()
mainGUI = do
  f <- frame [text := "statistics"]
  p <- panel f []
  txtN <- textEntry p [text := "10" , alignment := AlignRight]
  txtR <- textEntry p [text :=  "500" , alignment := AlignRight]
  txtA <- textEntry p [text := "0.005"  , alignment := AlignRight]
  draw <- button p [ text := "draw gragh"
                    ,on command := drawMode f
                    ,clientSize := sz 100 32]
  sim1 <- button p [ text := "simulate"
                    ,on command := simulateF f txtN txtR
                    ,clientSize := sz 100 32]
  sim2 <- button p [ text := "verify"
                    ,on command := simulateAssay f txtA txtN
                    ,clientSize := sz 80 32]
  set p [layout := (column 2 [
                     row 3 [ minsize (sz 100 32) $ label "事象数"
                            ,minsize (sz 100 32) $ label "試行回数"
                            ,minsize (sz 100 32) $ label "有意水準"]
                    ,row 6 [ minsize (sz 100 32) $ widget txtN
                            ,minsize (sz 100 32) $ widget txtR
                            ,minsize (sz 100 32) $ widget txtA
                            ,widget draw , widget sim1 , widget sim2]])]
  set f [ layout := fill $ widget p
         ,clientSize := sz 650 80]

--ソースコードから単語帳を取得
simulateAssay :: Window a -> TextCtrl () -> TextCtrl () -> IO()
simulateAssay f txtA txtN = do
    a <- liftM read (get txtA text) :: IO Double
    rank <- liftM read (get txtN text) :: IO Double
    file <- choseFile f ".hsファイルを選択" "haskell" ["*.hs","*.lhs"]
    case file of
        Nothing -> infoDialog f "ERROR" "ファイルを選択してください"
        Just path -> readFile path >>= run startRead
                        >>= showAssayed.assay rank a

showAssayed :: (Bool,Double,Double) -> IO()
showAssayed (b,x,chi) = do
    f <- frame [text := "検定結果"]
    p <- panel f []
    txt <- textEntry p [text := assayed b , enabled := False ]
    set p [layout := (column 1 [
                    row 3 [ minsize (sz 100 60) $ widget txt
                           ,minsize (sz 100 60) $ label ("X^2値:"++show x)
                           ,minsize (sz 100 60) $ label ("棄却域:"++show chi)]
                    ])]
    set f [layout := fill $ widget p , clientSize := sz 400 100]

assayed True = "帰無仮説採択"
assayed False = "帰無仮説棄却"

--描画モード
drawMode :: Window a ->  IO ()
drawMode f = do
  filename <- choseFile f "描画元ファイルを選択" "data plot" ["*.data"]
  case filename of
    Just file -> drawData file
    Nothing   -> infoDialog f "ERROR" "NO FILE CHOSEN!"

--ファイル選択
choseFile :: Window a -> String -> String -> [String] -> IO (Maybe FilePath)
choseFile f str ftype ex = fileOpenDialog f True True str [(ftype,ex)] "" ""
--事象数を受け取り、算出したχ^2分布をファイルに書き出す
simulateF :: Window a  -> TextCtrl () -> TextCtrl () -> IO()
simulateF f tN tR = do
    file <- fileSaveDialog f True True "save data" [("data",["*.data"])] "" ""
    t  <- (getCurrentTime >>= return.utctDayTime)
    n  <- liftM read (get tN text) :: IO Double
    rn <- liftM read (get tR text) :: IO Double
    case file of
        Just filename -> do
                     h <- openFile filename WriteMode
                     hPutStr h $ callSimulate n rn $ timeToInt t
                     hClose h >> infoDialog f "complete" "完了しました"
        Nothing -> infoDialog f "ERROR" "ファイル名を入力してください"

timeToInt :: DiffTime -> Int
timeToInt = floor.read.init.show

callSimulate :: Double -> Double -> Int -> String
callSimulate n rn time = simulate (rn,floor rn) n (zip rs1 rs2) where
    rs1 = mkRands time
    rs2 = mkRands $ time + 1

--点をplot(with Gloss)
drawData :: FilePath -> IO()
drawData file = do
    h <- openFile file ReadMode
    hGetContents h >>= plotDots [PNG file' , XRange (0,40)].unFormat
    hClose h
    bm <- bitmapCreateFromFile file'
    size <- bitmapGetSize bm
    f <- frame [text := file ,on paint := onPaint bm ,clientSize := size]
    repaint f
    where
        file' = takeWhile (/= '.') file ++ ".png"
        onPaint bm dc viewArea = drawBitmap dc bm pointZero False []


--座標データをファイルに書き出す時
format :: [(Double,Double)] -> String
format = concat.map (\(x,y)-> show x ++ "\t" ++ show y ++ "\n")

unFormat' = map (\(x,y) -> (x,y+10)).unFormat

--ファイルデータを座標データに変換するとき
unFormat :: String -> [(Double,Double)]
unFormat = map ((\[x,y] -> (read x,read y)).words). lines
