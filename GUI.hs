import System.IO (readFile)
import Graphics.Gnuplot.Simple
import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import Data.Time.Clock
import Control.Monad
import ReadSource
import Statistics

main = start mainGUI

--UI
mainGUI :: IO()
mainGUI = do
  f <- frame [text := "statistics"]
  p <- panel f []
  txtN <- textEntry p [text := "1000" , alignment := AlignRight]
  txtR <- textEntry p [text :=  "500" , alignment := AlignRight]
  txtA <- textEntry p [text := "0.005"  , alignment := AlignRight]
  draw <- button p [ text := "draw gragh"
                    ,on command := drawMode f
                    ,clientSize := sz 100 32]
  sim1 <- button p [ text := "simulate"
                    ,on command := simulateF f txtN txtR
                    ,clientSize := sz 100 32]
  sim2 <- button p [ text := "from .hs file"
                    ,on command := getFromFile f txtA
                    ,clientSize := sz 100 32]
  set p [layout := (column 2 [
                     row 3 [ minsize (sz 100 32) $ label "事象数"
                            ,minsize (sz 100 32) $ label "試行回数"
                            ,minsize (sz 100 32) $ label "有意水準"]
                    ,row 6 [ minsize (sz 100 32) $ widget txtN
                            ,minsize (sz 100 32) $ widget txtR
                            ,minsize (sz 100 32) $ widget txtA
                            ,widget draw , widget sim1 , widget sim2]])]
  set f [ layout := fill $ widget p
         ,clientSize := sz 650 70]

--ソースコードから単語帳を取得
getFromFile :: Window a -> TextCtrl () -> IO()
getFromFile f txt = do
    a <- get txt text
    file <- choseFile f ".hsファイルを選択" "haskell" ["*.hs","*.lhs"]
    case file of
        Nothing -> infoDialog f "ERROR" "ファイルを選択してください"
        Just path -> readFile path >>= run startRead >> return ()

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
