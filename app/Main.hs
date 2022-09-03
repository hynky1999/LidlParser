module Main where
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Expressions                    ( Program )
-- import           Interpreter
import           Parser

import           Interpreter
import           System.Environment
import           System.Exit

usage :: String
usage = "Usage: ./Main <input file>"




main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then do
            let filename = head args
            contents <- readFile filename
            let program = parse parseProgram contents
            case program of
                Left err -> do
                    putStrLn $ "Error: " ++ show err
                    exitFailure
                Right p -> do
                    result <- interpret (evalProgram p)
                                        (Store Map.empty Map.empty)
                    case result of
                        Left err -> do
                            putStrLn $ "Error: " ++ show err
                            exitFailure
                        Right res ->
                            putStrLn $ "Program finished with success."
        else putStrLn usage

