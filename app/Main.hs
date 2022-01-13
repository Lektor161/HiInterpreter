module Main
  ( main
  ) where


import HW3.Action
import HW3.Base ()
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline
import qualified Data.Set.Internal as S
import Control.Monad.Cont (liftIO)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi>"
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                case parse input of
                   (Left _)     -> outputStrLn "error parse"
                   (Right expr) -> do
                     result <- liftIO $ runHIO (eval expr) (S.singleton AllowRead)
                     case result of
                      (Left err)    -> outputStrLn $ show err
                      (Right value) -> outputStrLn $ show $ prettyValue value
                loop
--               Just input -> do outputStrLn $ show $ prettyValue $ evall input
--                                loop
