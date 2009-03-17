--
-- Module      : Settings
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Settings where

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef

activeAddPointMutationRate = 1500
activeAddPolygonMutationRate = 700
activeAlphaMutationRate = 1500
activeAlphaRangeMax = 60
activeAlphaRangeMin = 30
activeBlueMutationRate = 1500
activeBlueRangeMax = 255
activeBlueRangeMin = 0
activeGreenMutationRate = 1500
activeGreenRangeMax = 255
activeGreenRangeMin = 0
activeMovePointMaxMutationRate = 1500
activeMovePointMidMutationRate = 1500
activeMovePointMinMutationRate = 1500

activeMovePointRangeMid = 20.0
activeMovePointRangeMin = 3.0
activeMovePolygonMutationRate = 700
--activePointsMax = 1500
--activePointsMin = 0
activePointsPerPolygonMax = 10
activePointsPerPolygonMin = 3
activePolygonsMax = 255
activePolygonsMin = 0
activeRedMutationRate = 1500
activeRedRangeMax = 255
activeRedRangeMin = 0
activeRemovePointMutationRate = 1500
activeRemovePolygonMutationRate = 1500
--addPointMutationRate = 1500

maxWidth, maxHeight :: IORef Double
maxWidth = unsafePerformIO $ newIORef 100.0
maxHeight = unsafePerformIO $ newIORef 133.0