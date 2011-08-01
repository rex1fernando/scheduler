module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Rendering.Cairo

import qualified Class
import qualified TA
import GraphConstructor
import CSVReader

gui :: IO ()
gui = do
  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea


  drawingArea `onExpose` (\_ -> renderScene drawingArea)
  
  drawingArea `on` buttonPressEvent $ do
    (x,y) <- eventCoordinates
    liftIO $ putStrLn ("coordinates: " ++ (show x) ++ ", " ++ (show y))
    return True
  
  window `onDestroy` mainQuit

  windowSetDefaultSize window 640 480
  widgetShowAll window
  mainGUI

renderScene :: DrawingArea -> IO Bool
renderScene da = do
  dw <- widgetGetDrawWindow da
  gc     <- gcNew dw
  
  renderWithDrawable dw $ do setSourceRGBA 0 0 0 1.0
                             save  
                             translate 100 100
                             save
                             scale 1 0.5
                             arc 20 30 30 0 (2*pi) 
                             stroke
                             restore
                             translate (-5) 17
                             showText "HelloWorld"
                             restore
  return True
  
graph :: [(TA,Class)] -> [(TA, Class)] -> [Render a]
graph possiblePairings pairings = (map circleForTA tas) ++ (map circleForClass classes)
  where
    tas = sort $ nub $ map fst pairings
    classes = sort $ nub $ map snd pairings
    circle elem lst xOffset = do save
                                 translate xOffset 
    