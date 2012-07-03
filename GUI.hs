import Graphics.UI.WX
import qualified Graphics.Gloss as G
import Statistics
import Data.Time.Clock

(width,height) = (600,30)

main = start mainGUI

mainGUI :: IO()
mainGUI = do
  f <- frame [text := "statistics"]
  p <- panel f []
  textBoxN <- textEntry p [text := "1000" , alignment := AlignRight]
  textBoxFile <- textEntry p [text := ""  , alignment := AlignRight]
  draw <- button p [ text := "draw gragh"
                    ,on command := drawMode f 
                    ,clientSize := sz 100 height]
  siml <- button p [ text := "simulate"
                    ,on command := simulateF f 1000
                    ,clientSize := sz 80 height]
  set p [ layout := (column 1 [
                      row 4 [ minsize (sz 100 height) $ widget textBoxN
                             ,fill $ widget textBoxFile
                             ,widget draw , widget siml] ] ) ]
  set f [ layout := (fill $ widget p)
         ,clientSize := sz width height]

--描画モード
drawMode :: Window a -> IO ()
drawMode f= do
  filename <- choseFile f
  case filename of
    Just file -> infoDialog f "OK" file 
    Nothing   -> infoDialog f "ERROR" "NO FILE CHOSEN!"

--ファイル選択
choseFile :: Window a -> IO (Maybe FilePath)
choseFile f = fileOpenDialog f True True "Open data file"
                    [("Any File",["*.*"]),("plot data",["*.data"])] "" ""
--事象数を受け取り、算出したχ^2分布をファイルに書き出す
simulateF :: Window a -> Int -> IO()
simulateF form n = do
    t <- (getCurrentTime >>= return.utctDayTime)
    file <- fileSaveDialog form True True "save data" [("data",["*.data"])] "" ""
    case file of
        Just filename -> writeFile filename $ callSimulate n t
        Nothing       -> return()

timeToInt :: DiffTime -> Int
timeToInt = floor.read.init.show

callSimulate n time = simulateProb n rs1 rs2 where
    rs1 = mkRands.timeToInt $ time
    rs2 = mkRands.timeToInt $ time + 1

--点をplot(with Gloss)
drawData :: FilePath -> IO()
drawData file = do
  xs <- (readFile file >>= return.lines)
  G.display (G.InWindow "statistics" (200,200) (10,10)) G.white $ G.Circle 8.0
    where
      toPicture = map (toPos.words)
      toPos [x,y] = (read x,read y) :: (Float,Float)
