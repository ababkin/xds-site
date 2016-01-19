{-# LANGUAGE OverloadedStrings #-}

module Flash where

import Control.Lens ((.~), (&))
import Data.Monoid (mempty)
import Heist ((##), scInterpretedSplices, scCompiledSplices)
import Snap.Snaplet.Heist (HasHeist, addConfig, Heist)
import Snap.Snaplet (Snaplet, SnapletLens, Initializer)
import Snap.Snaplet.Session (SessionManager)
import Snap.Extras.FlashNotice (flashSplice, flashCSplice)


addFlashSplices 
  :: HasHeist b
  => Snaplet (Heist b)
  -> SnapletLens b SessionManager  
  -> Initializer b v ()
addFlashSplices h sess = addConfig h sc
  where
    sc = mempty & scInterpretedSplices .~ is
                & scCompiledSplices .~ cs
    is = "flash" ## flashSplice sess
    cs = "flash" ## flashCSplice sess
  

