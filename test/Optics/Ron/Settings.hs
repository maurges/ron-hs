{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels, DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Optics.Ron.Settings
    () where

import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import Data.Ron.Class (RonFlags, RonSettings)

makeFieldLabelsWith noPrefixFieldLabels ''RonFlags
makeFieldLabelsWith noPrefixFieldLabels ''RonSettings
