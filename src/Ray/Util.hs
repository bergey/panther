-- |

module Ray.Util where

import Numeric.Interval (Interval, (...))

posInfinity :: Floating a => a
posInfinity = 1/0

negInfinity :: Floating a => a
negInfinity = -1/0

nonNegative :: (Floating a, Ord a) => Interval a
nonNegative = 0...posInfinity
