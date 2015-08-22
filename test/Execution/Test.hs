{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Execution.Test (htf_thisModulesTests)  where

import           Test.Framework
import           Test.HUnit     (Assertion)

import           Execution


test_aggregate :: Assertion
test_aggregate = assertEqual (aggregate inp) oup
  where
    inp = [[[3],[4,5]],[[6],[9,7,8],[4,2,3,6]]]
    oup = [[3,6],[4,5,9,7,8],[4,2,3,6]]
