{-# OPTIONS_GHC -XFlexibleInstances -XUndecidableInstances -XIncoherentInstances #-}

module GUI where

import Data.List
import Data.Maybe
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Char

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Rendering.Cairo

import Class
import TA
import GraphConstructor
import CSVReader


gui :: IO ()
gui = do
  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  
  containerAdd window scrolledWindow
  
  (Right tas) <- readTAsFromFile "tas.csv"
  (Right classes) <- readClassesFromFile "classes.csv"
  
  let pp = possiblePairings classes tas []
  
  drawingArea `onExpose` (\_ -> renderScene drawingArea pp)
  
  drawingArea `on` buttonPressEvent $ do
    (x,y) <- eventCoordinates
    liftIO $ putStrLn ("coordinates: " ++ (show x) ++ ", " ++ (show y))
    return True
    
  widgetSetSizeRequest drawingArea 640 480
  
  
  scrolledWindowAddWithViewport scrolledWindow drawingArea
    
  window `onDestroy` mainQuit

  windowSetDefaultSize window 640 480
  widgetShowAll window
  mainGUI

renderScene :: DrawingArea -> [PossiblePairing] -> IO Bool
renderScene da pp = do
  dw <- widgetGetDrawWindow da
        
  let p = head pp
  let r = graph pp [p]
           
  foldr (>>) (return True) $ map (renderWithDrawable dw) r
                    
--renderScene da pp = (widgetGetDrawWindow da) >>= 
--                    (\dw -> foldr (>>) (return True) $ map (renderWithDrawable dw) (graph pp []))
                                                   

  -- renderWithDrawable dw $ do setSourceRGBA 0 0 0 1.0
  --                            save  
  --                            translate 100 100
  --                            save
  --                            scale 1 0.5
  --                            arc 20 30 30 0 (2*pi) 
  --                            stroke
  --                            restore
  --                            translate (-5) 17
  --                            showText "HelloWorld"
  --                            restore
  
class Graphable a where
  nodeText :: a -> String
  
instance Graphable Class where
  nodeText c = Class.name c
  
instance Graphable TA where
  nodeText t = TA.name t
  
instance (Graphable a) => Eq a where
  (==) a b = (nodeText a) == (nodeText b)
  
instance (Graphable a) => Ord a where
  (<=) a b = (map toLower (nodeText a)) <= (map toLower (nodeText b))

graph :: [PossiblePairing] -> [PossiblePairing] -> [Render (Maybe ())]
graph possiblePairings pairings = (map (runMaybeT . circleForTA) tas) ++ (map (runMaybeT . circleForClass) classes) ++ (map (runMaybeT . line) pairings)
  where
    tas = sort $ nub $ map snd possiblePairings
    classes = sort $ nub $ map fst possiblePairings
    
    circle :: (Graphable a) => a -> [a] -> Double -> MaybeT Render ()
    circle elem lst xOffset = do lift $ save
                                 lift $ setSourceRGBA 0 0 0 1.0
                                 yOffset <- MaybeT (return $ yOffsetFor elem lst)
                                 lift $ translate xOffset yOffset
                                 lift $ translate 0 (-15)
                                 lift $ save
                                 lift $ scale 2 0.5
                                 lift $ arc 20 30 30 0 (2*pi)
                                 lift $ stroke
                                 lift $ restore
                                 lift $ translate (-5) 17
                                 lift $ showText (nodeText elem)
                                 
    
    line :: PossiblePairing -> MaybeT Render ()                          
    line pairing = do lift $ save
                      lift $ setSourceRGBA 0 0 0 1.0
                      yOffsetTA <- MaybeT (return $ yOffsetFor (snd pairing) tas)
                      yOffsetClass <- MaybeT (return $ yOffsetFor (fst pairing) classes)
                      lift $ moveTo 200 yOffsetTA
                      lift $ lineTo 390 yOffsetClass
                      lift $ stroke
                      lift $ restore
                          
    circleForTA elem = circle elem tas 100
    circleForClass elem = circle elem classes 400
                              
    yOffsetFor :: (Eq a) => a -> [a] -> Maybe Double
    yOffsetFor elem lst = (elemIndex elem lst) >>= (\i -> return ((1.0+(fromIntegral i))*50))
