
module Main where

import Data.Char     (toUpper)
import Control.Monad (forever)
import System.IO     (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)



main = start mainGUI


makeNetworkDescription :: AddHandler Point -> Button () -> MomentIO ()
makeNetworkDescription addMouseEvent button = do
    eMouse <- fromAddHandler addMouseEvent
    bDownAt <- stepper (Point 0 0) eMouse
    let eMouse' = ((+1) <$ eMouse)
    bMouse <- accumB 0 eMouse' 
    eMouseChanged <- changes bMouse
    eDownAt <- changes bDownAt
    reactimate' $ fmap (\(Point x y) -> (set button [text := "x y" ++ (show x) ++ (show y)])) <$> eDownAt
 
mainGUI :: IO ()
mainGUI = do

    f <- frameFixed [] 
           
    -- panel for automatic tab management and the nice background
    p <- panelCreate f idAny rectNull 0
    t <- staticTextCreate p idAny "0" rectNull 0          
    b <- buttonCreate p idAny "" rectNull 0
   
  
   -- set the layout
    windowSetLayout f (fill (container p (margin 5 (column 5 [widget t, widget b]))))
       
    (addMouseEvent, fireMouse) <- newAddHandler
    network <- compile (makeNetworkDescription addMouseEvent b) 
    actuate network   
    
    set f [ text := "Bubble Sim", 
            bgcolor     := white, 
            layout      := space 500 500
           ]
           
    set b [text := "Click me !",         
            on click    := onMouseLeftClick b fireMouse
          ]
       
  
    return ()
    
    where 
        onMouseLeftClick :: Button () -> Handler Point -> Point -> IO ()
        onMouseLeftClick b h p = do
            h p
--            set b [text := "Clicked 1" ++ show p]          
            return ()
