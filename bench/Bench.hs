module Main where

import Criterion.Main (defaultMain)

import qualified File
import qualified Escape


main = defaultMain
    [ File.benchmark
    , Escape.benchmark
    ]
