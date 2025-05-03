module Distortion.DistortionM
  ( DistortionM(..)
  , DistortionF(..)
  ) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (class MonadTrans)

newtype DistortionM m a = DistortionM (Free (DistortionF m) a)

data DistortionF m a =
    Lift (m a)
  | 
