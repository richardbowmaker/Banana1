
module Main where

import Data.Char     (toUpper)
import Control.Monad (forever)
import System.IO     (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)



main = start mainGUI


makeNetworkDescription :: AddHandler () -> Button () -> MomentIO ()
makeNetworkDescription addMouseEvent button = do
    eMouse <- fromAddHandler addMouseEvent
    let eMouse' = ((+1) <$ eMouse)
    bMouse <- accumB 0 eMouse' 
    eMouseChanged <- changes bMouse
    reactimate' $ fmap (\n -> (set button [text := "Clicked " ++ show n])) <$> eMouseChanged
 
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
        onMouseLeftClick :: Button () -> Handler () -> Point -> IO ()
        onMouseLeftClick b h _ = do
            h ()
--            set b [text := "Clicked"]          
            return ()
