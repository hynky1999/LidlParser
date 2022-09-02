-- Tato možnost zapne všechny warningy:
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter where

import qualified Data.Map                      as Map

import           Control.Monad.IO.Class
import           Expressions
import           State




type RuntimeError = String


data Store = Store
    { storeVars :: Map.Map Id Value
    , storeFcs  :: Map.Map Id Func
    }
    deriving (Show, Eq)

newtype Interpreter a = Interpreter {runInterpreter :: State Store (Either RuntimeError (IO a))}

mapParser :: (a -> b) -> Interpreter a -> Interpreter b
mapParser f (Interpreter p) = Interpreter $ do
    result <- p
    case result of
        Left  err -> return $ Left err
        Right a   -> return $ Right $ fmap f a



instance Functor Interpreter where
    fmap = mapParser

returnParser :: a -> Interpreter a
returnParser x = Interpreter $ do
    return $ Right $ return x


bindInterpreter :: Interpreter a -> (a -> Interpreter b) -> Interpreter b
bindInterpreter (Interpreter p) f = Interpreter $ do
    result <- p
    case result of
        Right x   -> undefined

---- HOW to do this ???????
----- I Need to get a but I only have IO a



        Left  err -> return $ Left err

instance Monad Interpreter where
    return = returnParser
    (>>=)  = bindInterpreter

instance Applicative Interpreter where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x

getInterpreter :: Interpreter Store
getInterpreter = Interpreter $ do
    s <- get
    return $ Right $ return s

setInterpreter :: Store -> Interpreter ()
setInterpreter store = Interpreter $ do
    set store
    return $ Right $ return ()

modifyInterpreter :: (Store -> Store) -> Interpreter ()
modifyInterpreter f = Interpreter $ do
    store <- get
    set $ f store
    return $ Right $ return ()

raiseError :: RuntimeError -> Interpreter a
raiseError err = Interpreter $ do
    return $ Left err


interpret :: Interpreter a -> Store -> Either RuntimeError (IO a)
interpret (Interpreter p) store = snd $ runState p store

interpretDebug :: Interpreter a -> Store -> Store
interpretDebug (Interpreter p) store = fst $ runState p store

--------------------------------------------------------------------------------

evalExpr :: Expression -> Interpreter Value
evalExpr expr = case expr of
    RelExpression op exp1 exp2  -> evalRelExpression op exp1 exp2
    NumExpression op exp1 exp2  -> evalNumExpression op exp1 exp2
    VarExpression   var         -> evalVarExpression var
    ValueExpression val         -> return val
    UnaryExpression op exp1     -> evalUnaryExpression op exp1
    FunctionCallExpression call -> evalFunctionCall call


evalRelExpression :: RelOp -> Expression -> Expression -> Interpreter Value
evalRelExpression op exp1 exp2 = do
    val1 <- evalExpr exp1
    val2 <- evalExpr exp2
    case op of
        Eq  -> return $ BoolValue $ val1 == val2
        Neq -> return $ BoolValue $ val1 /= val2
        _   -> raiseError $ "Unsupported relational operator: " ++ show op


evalNumExpression :: NumOp -> Expression -> Expression -> Interpreter Value
evalNumExpression op exp1 exp2 = do
    v1 <- evalExpr exp1
    v2 <- evalExpr exp2
    evalNumOp op v1 v2

evalNumOp :: NumOp -> Value -> Value -> Interpreter Value


evalUnaryExpression :: UnaryOp -> Expression -> Interpreter Value
evalUnaryExpression op exp1 = do
    v <- evalExpr exp1
    evalUnaryOp op v

evalUnaryOp :: UnaryOp -> Value -> Interpreter Value


evalVarExpression :: Id -> Interpreter Value
evalVarExpression var = do
    store <- getInterpreter
    case Map.lookup var (storeVars store) of
        Just val -> return val
        Nothing  -> raiseError $ "Variable " ++ var ++ " not found"

-- Desnt allow to modify globals only locals currently
evalFunctionCall :: FunctionCall -> Interpreter Value
evalFunctionCall (FunctionCall callName callArgs) = do
    store <- getInterpreter
    case Map.lookup callName (storeFcs store) of
        Just (Func funcArgs funcBody retName retVal) ->
            if length funcArgs /= length callArgs
                then raiseError "Invalid number of arguments"
                else do
                    -- Evaluate before resetting stack
                    callArgsEvaluated <- mapM evalExpr callArgs
                    -- Reset vars stack
                    setInterpreter Store { storeVars = Map.empty
                                         , storeFcs  = storeFcs store
                                         }
                    -- Define func arguments
                    evalBlock $ map (uncurry Define) funcArgs

                    -- Assign func arguments call arguments
                    evalBlock $ zipWith
                        (\name expr -> Assign name (ValueExpression expr))
                        (map fst funcArgs)
                        callArgsEvaluated

                    -- Insert Typed return value
                    evalStatement $ Define retName retVal
                    -- Evaluated body
                    evalBlock funcBody
                    returnValue <- evalVarExpression retName
                    -- Revert stack
                    setInterpreter store
                    return returnValue

        Nothing -> raiseError $ "Function " ++ callName ++ " not found"

evalFunctionDef :: Id -> Func -> Interpreter ()
evalFunctionDef name fc = do
    -- Should be more complex to allow overloading and stuff
    modifyInterpreter
        $ \store -> store { storeFcs = Map.insert name fc (storeFcs store) }

--------------------------------------------------------------------------------

evalStatement :: Statement -> Interpreter ()
evalStatement stmt = case stmt of
    Assign name expr          -> evalAssign name expr
    Define name val           -> evalDefine name val
    If cond ifBlock elseBlock -> evalIf cond ifBlock elseBlock
    While cond block          -> evalWhile cond block
    FunctionCallStmt fcCall   -> evalFunctionCall fcCall >> return ()
    FunctionDef name fc       -> evalFunctionDef name fc




writeIO :: String -> Interpreter ()
writeIO str = Interpreter $ do
    return $ Right $ putStrLn str


evalBuiltinCall :: [Expression] -> Interpreter Value
evalBuiltinCall exprs = do
    vals <- mapM evalExpr exprs
    mapM_ writeIO (map show vals)
    return Null


evalAssign :: Id -> Expression -> Interpreter ()
evalAssign name expr = do
    val   <- evalExpr expr
    store <- getInterpreter
    case Map.lookup name (storeVars store) of
        Nothing     -> raiseError $ "Variable " ++ name ++ "not defined"
        Just varVal -> do
            -- strict Type check
            if typeOf varVal == typeOf val
                then
                    modifyInterpreter
                        $ \store -> store
                              { storeVars = Map.insert name
                                                       val
                                                       (storeVars store)
                              }
                else
                    raiseError
                    $  "Cannot assign "
                    ++ show (typeOf val)
                    ++ " to "
                    ++ show (typeOf varVal)

evalDefine :: Id -> Type -> Interpreter ()
evalDefine name typeOfVal = do
    store <- getInterpreter
    case Map.lookup name (storeVars store) of
        Just _  -> raiseError $ "Variable " ++ name ++ " already defined"
        Nothing -> modifyInterpreter $ \store -> store
            { storeVars = Map.insert name
                                     (defaultValue typeOfVal)
                                     (storeVars store)
            }

evalIf :: Expression -> Block -> Block -> Interpreter ()
evalIf cond ifBlock elseBlock = do
    val <- evalExpr cond
    case val of
        BoolValue True  -> evalBlock ifBlock
        BoolValue False -> evalBlock elseBlock
        _               -> raiseError "If condition must be boolean"

evalWhile :: Expression -> Block -> Interpreter ()
evalWhile cond block = do
    val <- evalExpr cond
    case val of
        BoolValue True  -> evalBlock block >> evalWhile cond block
        BoolValue False -> return ()
        _               -> raiseError "While condition must be boolean"

--------------------------------------------------------------------------------

evalBlock :: Block -> Interpreter ()
evalBlock stmts = do
    mapM_ evalStatement stmts


evalProgram :: Program -> Interpreter ()
evalProgram (Program _ stmts) = evalBlock stmts


--------------------------------------------------------------------------------
-- Unfortunately it has to be defined function by function and not type by type :/

-- Int definitions of operations
evalNumOp op (IntValue this) other = case other of
    IntValue o -> case op of
        Add -> return $ IntValue $ this + o
        Mul -> return $ IntValue $ this * o
        Sub -> return $ IntValue $ this - o
        Div -> return $ IntValue $ this `div` o
    _ -> raiseError "Invalid operation"

evalNumOp _ (BoolValue _) _ = raiseError "Invalid operation"

evalUnaryOp op (IntValue this) = case op of
    Neg -> return $ IntValue $ negate this
    Pos -> return $ IntValue this
    _   -> raiseError "Invalid operation"



-- Bool definitions of operations
evalUnaryOp _ (BoolValue _) = raiseError "Invalid operation"
