module Var where
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict
import Data.Map (Map, insert, empty)
import qualified Data.Map as Map
import Data.List (foldl')


type Var m = StateT (Map String Double) m

fetchValue:: Monad m => String->Var m (Maybe Double)
fetchValue name = do
	varmap <- get
	return $ Map.lookup name varmap

setValue :: Monad m => String->Double->Var m ()
setValue name val=do
  varmap <- get
  put $ insert name val varmap
  return ()

mainLoop :: StateT (Map String Double) IO ()
mainLoop = do
  line <- lift getLine :: StateT (Map String Double) IO String
  let {
    wds = words line;
  }
  if wds == [] || wds == ["exit"]
    then return ()
    else do
      let {
        wds = words line;
        name = wds !! 0;
      } in
        (case length wds of
          0 -> return ()
          1 -> fetchValue name >>= return . (\x -> name ++ "=" ++ show x) >>= lift . putStrLn
          _ -> let value = read (wds !! 1) :: Double in value `seq` setValue name value >> lift (putStrLn (name ++ "<-" ++ show value)));
      mainLoop

main :: IO ()
main = runStateT mainLoop empty >> return ()
