import Graphics.UI.WX
import qualified Graphics.Gloss as G
import Statistics

(width,height) = (600,50)

main = start mainGUI

mainGUI :: IO()
mainGUI = do
  f <- frame [text := "statistics"]
  p <- panel f []
  textBoxN <- textEntry p [text := "1000" , alignment := AlignRight]
  textBoxFile <- textEntry p [text := ""  , alignment := AlignRight]
  draw <- button f [ text := "draw gragh"
                    ,on command := drawMode f 
                    ,clientSize := sz 80 30]
  siml <- button f [ text := "simulate"
                    ,on command := simulateF f 1000
                    ,clientSize := sz 80 30]
  set f [ layout := margin 4 $ container p $ floatCenter $
            row 5 [ minsize (sz 60 30) $ widget textBoxN
                   ,minsize (sz 100 30) $ widget textBoxFile
                   ,widget siml
                   ,widget draw
                   ]
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

--点をplot(with Gloss)
drawData :: FilePath -> IO()
drawData file = do
  xs <- (readFile file >>= return.lines)
  G.display (G.InWindow "statistics" (200,200) (10,10)) G.white $ G.Circle 8.0
    where
      toPicture = map (toPos.words)
      toPos [x,y] = (read x,read y) :: (Float,Float)
