module Main where

import Control.Monad.ST (ST, runST)
import Lib

main :: IO ()
main = do
  print $
    runST $ do
      empty >>= toList :: ST s [Int]
  print $
    runST $ do
      let xs = [1 .. 10]
      d <- fromList xs
      union 1 2 d
      union 3 1 d
      union 5 6 d
      union 7 8 d
      union 7 6 d
      union 10 9 d
      -- (,) <$> toList d <*> traverse (`find` d) xs
      l <- toDebugList d
      union 3 6 d
      l' <- toDebugList d
      pure (l, l')
