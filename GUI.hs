import Graphics.Gnuplot.Simple
import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import Statistics
import Data.Time.Clock
import Control.Monad

(width,height) = (600,60)

main = start mainGUI

--UI
--サンプル数（事象数）、試行数、データ保存、グラフ描画
mainGUI :: IO()
mainGUI = do
  f <- frame [text := "statistics"]
  p <- panel f []
  txtN <- textEntry p [text := "1000" , alignment := AlignRight]
  txtR <- textEntry p [text :=  "500" , alignment := AlignRight]
  txtFile <- textEntry p [text := ""  , alignment := AlignRight]
  draw <- button p [ text := "draw gragh"
                    ,on command := drawMode f
                    ,clientSize := sz 100 25]
  siml <- button p [ text := "simulate"
                    ,on command := simulateF f txtN txtR
                    ,clientSize := sz 80 25]
  set p [ layout := (column 2 [
                       row 2 [ minsize (sz 100 25) $ label "事象数"
                              ,minsize (sz 100 25) $ label "試行回数"]
                      ,row 5 [ minsize (sz 100 25) $ widget txtN
                              ,minsize (sz 100 25) $ widget txtR
                              ,minsize (sz 140 25)$ widget txtFile
                              ,widget draw , widget siml] ] )]
  set f [ layout := fill $ widget p
         ,clientSize := sz width height]

--描画モード
drawMode :: Window a ->  IO ()
drawMode f = do
  filename <- choseFile f
  case filename of
    Just file -> drawData file
    Nothing   -> infoDialog f "ERROR" $ "NO FILE CHOSEN!"

--ファイル選択
choseFile :: Window a -> IO (Maybe FilePath)
choseFile f = fileOpenDialog f True True "Open data file"
                    [("Any File",["*.*"]),("plot data",["*.data"])] "" ""
--事象数を受け取り、算出したχ^2分布をファイルに書き出す
simulateF :: Window a  -> TextCtrl () -> TextCtrl () -> IO()
simulateF f tN tR = do
    file <- fileSaveDialog f True True "save data" [("data",["*.data"])] "" ""
    t  <- (getCurrentTime >>= return.utctDayTime)
    n  <- liftM read (get tN text) :: IO Int
    rn <- liftM read (get tR text) :: IO Int
    case file of
        Just filename -> writeFile filename $ callSimulate n rn $ timeToInt t
        Nothing       -> infoDialog f "事象数" $ show n -- "ERROR" "ファイル名を入力してください"

timeToInt :: DiffTime -> Int
timeToInt = floor.read.init.show

callSimulate :: Int -> Int -> Int -> String
callSimulate n rn time = simulateProb n rs1 rs2 where
    rs1 = mkRands time
    rs2 = mkRands $ time + 1

--点をplot(with Gloss)
drawData :: FilePath -> IO()
drawData file = do
    readFile file >>= plotDots [PNG file'].unFormat
    bm <- bitmapCreateFromFile file'
    size <- bitmapGetSize bm
    f <- frame [text := file ,on paint := onPaint bm ,clientSize := size]
    repaint f
    where
        file' = takeWhile (/= '.') file ++ ".png"
        onPaint bm dc viewArea = drawBitmap dc bm pointZero False []
