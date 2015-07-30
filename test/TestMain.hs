{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Problems.Test
import                   Test.Framework

main = htfMain htf_importedTests
