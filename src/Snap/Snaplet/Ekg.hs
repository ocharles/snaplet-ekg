{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Ekg
    ( Ekg, ekgInit, wrapRequestStats ) where

import Control.Applicative
import Control.Concurrent (killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.Lens.Common (Lens)
import Snap (Snaplet, makeSnaplet, onUnload, with, Initializer, wrapSite, SnapletInit)
import System.Remote.Counter as Counter
import System.Remote.Gauge as Gauge
import System.Remote.Monitoring (forkServer, getCounter, getGauge, forkServer, serverThreadId)

data Ekg = Ekg { ekgRequestsServed :: Counter.Counter
               , ekgRequestsActive :: Gauge.Gauge
               }

ekgInit :: SnapletInit b Ekg
ekgInit = makeSnaplet "ekg" "Ekg" Nothing $ do
  server <- liftIO (forkServer "localhost" 9876)
  onUnload (killThread $ serverThreadId server)
  Ekg <$> liftIO (getCounter "Total requests served" server)
      <*> liftIO (getGauge "Requests active" server)

wrapRequestStats :: Lens b (Snaplet Ekg) -> Initializer b b ()
wrapRequestStats ekg = wrapSite (\h -> pre >> h >> post)
  where post = with ekg $ do
          gets ekgRequestsServed >>= liftIO . Counter.inc
          gets ekgRequestsActive >>= liftIO . Gauge.dec
        pre = with ekg $
          gets ekgRequestsActive >>= liftIO . Gauge.inc
