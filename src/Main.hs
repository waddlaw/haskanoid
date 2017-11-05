{-# LANGUAGE CPP #-}
import Control.Applicative ((<$>))
import Control.Monad.IfElse
import FRP.Yampa as Yampa

import GamePlay
import Input

#ifdef sdl
import Display
import Graphics.UI.Extra.SDL
#endif

#ifdef sdl2
import DisplaySDL2
import Game.Clock.SDL2
#endif

#ifdef ghcjs
import Control.Concurrent
import DisplayGHCJS
import GHCJSNow
import System.Mem
#endif

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  awhen res $ \res' -> do
    renderingCtx <- initGraphs res'
    reactimate (senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' e renderingCtx >> return False) -- GHCJS: (\_ e -> render res' e >> threadDelay 1000 >> return False)
               wholeGame
 
