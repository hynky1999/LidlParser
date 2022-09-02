module Main where
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Expressions                    ( Program )
import           Interpreter
import           Parser
import           System.Environment
import           System.Exit

usage :: String
usage = "Usage: ./Main <input file>"


handleParserError :: Either ParseError a -> IO a
handleParserError (Left  err ) = print err >> exitFailure
handleParserError (Right prog) = return prog

handleInterpreterError :: Either RuntimeError a -> IO ()
handleInterpreterError (Left  err) = print err >> exitFailure
handleInterpreterError (Right _  ) = return ()


main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then do
            let filename = head args
            contents <- readFile filename
            program  <- handleParserError $ parse parseProgram contents
            _        <- handleInterpreterError $ interpret
                (evalProgram program)
                (Store Map.empty Map.empty)
            putStrLn "Program executed successfully"
        else putStrLn usage

