

import Test.QuickCheck
import Data.List


testRev :: [Int] -> Bool
testRev r = r == (reverse $ reverse r)
