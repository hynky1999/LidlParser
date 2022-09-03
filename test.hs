import           Expressions
import           Parser




main :: IO ()
main = do
    putStrLn "Enter an expression:"
    input <- getLine
    let res = parse parseExpression input
    case res of
        Left  err  -> putStrLn $ "Error: " ++ show err
        Right expr -> do
            putStrLn $ "Parsed expression: " ++ show expr




