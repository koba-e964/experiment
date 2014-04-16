module Main where

import Control.Monad (forM_, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Lazy (runStateT)
import qualified Data.Map as Map
import HaskCalc
import Var (Var)

mainLoop :: [String] -> Var IO ()
mainLoop [] = return ()
mainLoop (x:xs) = do
  res <- evalString x
  lift $ print res
  mainLoop xs

-- main interactively interprets expressions in stdin and shows the results.
main :: IO ()
main = do
  lines <- liftM lines getContents
  runStateT (mainLoop lines) Map.empty
  return ()