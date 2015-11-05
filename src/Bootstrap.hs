{-# LANGUAGE OverloadedStrings #-}

module Bootstrap where

import Control.Lens
import Data.Monoid (mempty)
import Heist ((##), scInterpretedSplices, scCompiledSplices)
import Snap.Snaplet.Heist (HasHeist, addConfig, Heist, getHeistState)
import Snap.Snaplet (Snaplet, SnapletLens, Initializer)
import Snap.Snaplet.Session (SessionManager)
import Snap.Extras.FlashNotice (flashSplice, flashCSplice)
import Heist.Interpreted (textSplice, lookupSplice)


{- addBootstrapSplices -}
  {- :: HasHeist b -}
  {- => Snaplet (Heist b) -}
  {- -> Initializer b v () -}
{- addBootstrapSplices h = -}
  {- addConfig h sc -}

  {- where -}
    {- sc = mempty & scInterpretedSplices .~ is -}
                {- [> & scCompiledSplices .~ cs <] -}
    {- is = do -}
      {- hs <- getHeistState -}
      {- case lookupSplice "type" hs of -}
        {- Just "error" -> -}
          {- "bootstrap-type" ## textSplice "danger"  -}
        {- Just x -> -}
          {- "bootstrap-type" ## textSplice x -}
        {- Nothing -> -}
          {- return () -}

    {- cs = "bootstrap-type" ## textSplice "danger" -}


