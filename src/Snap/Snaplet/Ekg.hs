{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Ekg
    ( Ekg, ekgInit, wrapRequestStats ) where

import Control.Applicative
import Control.Concurrent (killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Snap (makeSnaplet, onUnload, wrapSite, SnapletInit, Handler)
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
  wrapSite wrapRequestStats
  Ekg <$> liftIO (getCounter "Total requests served" server)
      <*> liftIO (getGauge "Requests active" server)

wrapRequestStats :: Handler b Ekg a -> Handler b Ekg a
wrapRequestStats h = pre *> h <* post
  where post = do
          gets ekgRequestsServed >>= liftIO . Counter.inc
          gets ekgRequestsActive >>= liftIO . Gauge.dec
        pre =
          gets ekgRequestsActive >>= liftIO . Gauge.inc
