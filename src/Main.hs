{-# LANGUAGE CPP #-}

-- External imports
import Control.Applicative       ((<$>))
import Control.Exception.Extra   (catchAny)
import Control.Monad.IfElse      (awhen)
import FRP.Yampa                 as Yampa (reactimate)
import Game.Resource.Manager.Ref (loadResources)
import Game.Resource.Spec        (localizeResourceSpec)
import Paths_haskanoid           (getDataFileName)

-- Internal imports
import GamePlay (wholeGame)
import Input    (initializeInputDevices, senseInput)

-- Device specific imports
#if defined(sdl) || defined(sdl2)
import Display    (gameResourceSpec, initGraphs, initializeDisplay, render)
import Game.Clock (initializeTimeRef, milisecsToSecs, senseTimeRef)
#endif

#ifdef ghcjs
import DisplayGHCJS
import GHCJSNow
-- import Control.Concurrent
-- import System.Mem
#endif

-- TODO: Use MaybeT or ErrorT to report errors
-- | Start game and keep game loop alive.
main :: IO ()
main = (`catchAny` print) $ do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  resSpec       <- localizeResourceSpec getDataFileName gameResourceSpec
  res           <- loadResources resSpec

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
