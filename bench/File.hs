module File (benchmark) where

import Criterion.Main (bgroup, bench, nfIO)
import Data.Ron.Deserialize (loadFile)

benchFile name =
    bench ("Read " <> name) $ nfIO (loadFile $ "bench/data/" <> name)

benchmark = bgroup "Load file"
    [ benchFile "canada.ron"
    , benchFile "canada.json"
    ]
