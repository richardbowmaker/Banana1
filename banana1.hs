module Main where

import Data.Char     (toUpper)
import Control.Monad (forever)
import System.IO     (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)



main = start mainGUI


makeNetworkDescription :: AddHandler Point -> AddHandler Point -> Button () -> MomentIO ()
makeNetworkDescription addMouseClick addMouseDrag button = do

    eMouseClick <- fromAddHandler addMouseClick
    bDownAt <- stepper (Point 0 0) eMouseClick

    eMouseDrag <- fromAddHandler addMouseClick
    bDragTo <- stepper (Point 0 0) eMouseDrag
    


    eDownAt <- changes bDownAt
    reactimate' $ fmap (\(Point x y) -> (set button [text := "x y (1) " ++ (show x) ++ " " ++(show y)])) <$> eDownAt
 
mainGUI :: IO ()
mainGUI = do

    f <- frameFixed [] 
           
    -- panel for automatic tab management and the nice background
    p <- panelCreate f idAny rectNull 0
    t <- staticTextCreate p idAny "0" rectNull 0          
    b1 <- buttonCreate p idAny "" rectNull 0
    b2 <- buttonCreate p idAny "" rectNull 0
   
    set f [ text := "Bubble Sim", 
            bgcolor     := white, 
            layout      := space 500 500
          ]
  
   -- set the layout
    windowSetLayout f (fill (container p (margin 5 (column 5 [widget t, widget b1]))))
       
    (addMouseClick, fireMouseClick) <- newAddHandler
    (addMouseDrag, fireMouseDrag) <- newAddHandler
    network <- compile (makeNetworkDescription addMouseClick addMouseDrag b1) 
    actuate network   
    
            
    set p [     
            on click    := onMouseClick fireMouseClick,
            on drag     := onMouseDrag fireMouseDrag
          ] 
 

 
    return ()
    

    where 
        onMouseClick :: Handler Point -> Point -> IO ()
        onMouseClick h p = do
            h p
            return ()
            
        onMouseDrag :: Handler Point -> Point -> IO ()
        onMouseDrag h p = do
            h p
            return ()            