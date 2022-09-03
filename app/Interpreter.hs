-- Tato možnost zapne všechny warningy:
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import qualified Data.Map                      as Map

import           Data.Maybe                     ( listToMaybe )
import           Expressions
import           State
import           Text.Read




type RuntimeError = String



data Stack = Stack
    { vars :: Map.Map Id Value
    , fcs  :: Map.Map Id Func
    }
    deriving (Show, Eq)

emptyStack :: Stack
emptyStack = Stack Map.empty Map.empty

emptyStore :: Store
emptyStore = Store emptyStack emptyStack



data Store = Store
    { globalStack :: Stack
    , localStack  :: Stack
    }
    deriving (Show, Eq)

modifyStore :: Scope -> Store -> (Stack -> Stack) -> Store
modifyStore Global store f = store { globalStack = f (globalStack store) }
modifyStore Local  store f = store { localStack = f (localStack store) }

lookUpAllStacks
    :: Store -> Id -> (Scope -> Store -> Id -> Maybe a) -> Maybe (a, Scope)
lookUpAllStacks store id' lFc = foldr go Nothing [Global, Local]
  where
    go scope acc = case acc of
        Just _ -> acc
        Nothing ->
            let res = lFc scope store id'
            in  (\val -> Just (val, scope)) =<< res


fcLookUpAllStacks :: Store -> Id -> Maybe (Func, Scope)
fcLookUpAllStacks store id' = lookUpAllStacks store id' fcLookUp

varLookUpAllStacks :: Store -> Id -> Maybe (Value, Scope)
varLookUpAllStacks store id' = lookUpAllStacks store id' varLookUp

varLookUp :: Scope -> Store -> Id -> Maybe Value
varLookUp scope store id' = Map.lookup id' (vars $ storeToStack scope store)


fcLookUp :: Scope -> Store -> Id -> Maybe Func
fcLookUp scope store id' = Map.lookup id' (fcs $ storeToStack scope store)


storeToStack :: Scope -> Store -> Stack
storeToStack Local  s = localStack s
storeToStack Global s = globalStack s




newtype Interpreter a = Interpreter { runInterpreter :: StateT Store (ExceptT RuntimeError IO) a }
  deriving newtype (Functor, Applicative, Monad) -- magie, StateT ... už je monáda, tak to jen práskneme Haskellu

liftStateOp :: StateT Store (ExceptT RuntimeError IO) a -> Interpreter a
liftStateOp = Interpreter

getInterpreter :: Interpreter Store
getInterpreter = liftStateOp get'

setInterpreter :: Store -> Interpreter ()
setInterpreter store = liftStateOp $ set store

modifyInterpreter :: (Store -> Store) -> Interpreter ()
modifyInterpreter f = liftStateOp $ modify f

raiseError :: RuntimeError -> Interpreter a
raiseError err = Interpreter $ StateT $ \_ -> ExceptT $ return $ Left err


liftIO :: IO a -> Interpreter a
liftIO io = Interpreter $ StateT $ \s -> ExceptT $ do
    result <- io
    return $ Right (s, result)

interpret :: Interpreter a -> Store -> IO (Either RuntimeError a)
interpret (Interpreter p) store = runExceptT $ do
    result <- runStateT p store
    return $ snd result

interpretDebug :: Interpreter a -> Store -> IO (Either RuntimeError Store)
interpretDebug (Interpreter p) store = runExceptT $ do
    result <- runStateT p store
    return $ fst result

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
    evalRelOp op val1 val2

evalNumExpression :: NumOp -> Expression -> Expression -> Interpreter Value
evalNumExpression op exp1 exp2 = do
    v1 <- evalExpr exp1
    v2 <- evalExpr exp2
    evalNumOp op v1 v2

evalNumOp :: NumOp -> Value -> Value -> Interpreter Value
evalRelOp :: RelOp -> Value -> Value -> Interpreter Value


evalUnaryExpression :: UnaryOp -> Expression -> Interpreter Value
evalUnaryExpression op exp1 = do
    v <- evalExpr exp1
    evalUnaryOp op v

evalUnaryOp :: UnaryOp -> Value -> Interpreter Value


evalVarExpression :: Id -> Interpreter Value
evalVarExpression var = do
    store <- getInterpreter
    case varLookUpAllStacks store var of
        Just (val, _) -> return val
        Nothing       -> raiseError $ "Variable " ++ var ++ " not found"

-- Desnt allow to modify globals only locals currently
evalFunctionCall :: FunctionCall -> Interpreter Value
evalFunctionCall (FunctionCall callName callArgs) = do
    store <- getInterpreter
    case fcLookUpAllStacks store callName of
        Just (Func funcArgs funcBody retName retVal, _) ->
            if length funcArgs /= length callArgs
                then raiseError "Invalid number of arguments"
                else do
                    -- Evaluate before resetting stack
                    callArgsEvaluated <- mapM evalExpr callArgs
                    -- Reset local stack
                    setInterpreter Store { globalStack = globalStack store
                                         , localStack  = emptyStack
                                         }
                    -- Define func arguments
                    evalBlock Local $ map (uncurry DefVar) funcArgs

                    -- Assign func arguments call arguments
                    evalBlock Local $ zipWith
                        (\name expr -> Assign name (ValueExpression expr))
                        (map fst funcArgs)
                        callArgsEvaluated

                    -- Insert Typed return value
                    evalStatement Local $ DefVar retName retVal
                    -- Evaluated body
                    evalBlock Local funcBody
                    returnValue <- evalVarExpression retName
                    -- Revert stack
                    setInterpreter store
                    return returnValue

        Nothing -> raiseError $ "Function " ++ callName ++ " not found"

evalDefFc :: Id -> Scope -> Func -> Interpreter ()
evalDefFc name scope fc = do
    store <- getInterpreter
    case fcLookUp scope store name of
        Just _  -> raiseError $ "Functoin " ++ name ++ " already defined"
        Nothing -> modifyInterpreter $ \s -> modifyStore scope s
            $ \stack -> stack { fcs = Map.insert name fc (fcs stack) }

--------------------------------------------------------------------------------

evalStatement :: Scope -> Statement -> Interpreter ()
evalStatement scope stmt = case stmt of
    Assign name expr          -> evalAssign name expr
    DefVar name val           -> evalDefVar name scope val
    DefFc  name fc            -> evalDefFc name scope fc
    If cond ifBlock elseBlock -> evalIf scope cond ifBlock elseBlock
    While cond block          -> evalWhile scope cond block
    FunctionCallStmt fcCall   -> evalFunctionCall fcCall >> return ()
    IOInteropStmt op exprs    -> evalInterOpIO op exprs
    CompoundStmt stmts        -> evalBlock scope stmts


evalInterOpIO :: IOInterop -> [Expression] -> Interpreter ()
evalInterOpIO op exprs = case op of
    Print -> do
        values <- mapM evalExpr exprs
        liftIO $ mapM_ print values
    Read -> do
        if length exprs /= 1
            then raiseError "Invalid number of arguments"
            else case listToMaybe exprs of
                Nothing -> raiseError "Read requires variable name"
                Just (VarExpression var) -> do
                    value <- liftIO $ getLine
                    -- This would be string value if we supported it
                    let intValue = readMaybe value :: Maybe Int
                    case intValue of
                        Just int ->
                            evalAssign var (ValueExpression $ IntValue int)
                        Nothing -> raiseError "Invalid input"
                Just _ -> raiseError "Read requires variable name"


evalAssign :: Id -> Expression -> Interpreter ()
evalAssign name expr = do
    val   <- evalExpr expr
    store <- getInterpreter
    case varLookUpAllStacks store name of
        Nothing -> raiseError $ "Variable " ++ name ++ " not defined"
        Just (varVal, scope) -> do
            -- strict Type check

            if typeOf varVal == typeOf val
                then modifyInterpreter $ \s -> modifyStore scope s $ \stack ->
                    stack { vars = Map.insert name val $ vars stack }
                else
                    raiseError
                    $  "Cannot assign "
                    ++ show (typeOf val)
                    ++ " to "
                    ++ show (typeOf varVal)

evalDefVar :: Id -> Scope -> Type -> Interpreter ()
evalDefVar name scope typeOfVal = do
    store <- getInterpreter
    case varLookUp scope store name of
        Just _  -> raiseError $ "Variable " ++ name ++ " already defined"
        Nothing -> modifyInterpreter $ \s ->
            modifyStore scope s
                $ \stack -> stack
                      { vars = Map.insert name
                                          (defaultValue typeOfVal)
                                          (vars stack)
                      }

evalIf :: Scope -> Expression -> Block -> Block -> Interpreter ()
evalIf scope cond ifBlock elseBlock = do
    val <- evalExpr cond
    case val of
        BoolValue True  -> evalBlock scope ifBlock
        BoolValue False -> evalBlock scope elseBlock
        _               -> raiseError "If condition must be boolean"

evalWhile :: Scope -> Expression -> Block -> Interpreter ()
evalWhile scope cond block = do
    val <- evalExpr cond
    case val of
        BoolValue True  -> evalBlock scope block >> evalWhile scope cond block
        BoolValue False -> return ()
        _               -> raiseError "While condition must be boolean"

--------------------------------------------------------------------------------

evalBlock :: Scope -> Block -> Interpreter ()
evalBlock scope stmts = do
    mapM_ (evalStatement scope) stmts



evalProgram :: Program -> Interpreter ()
evalProgram (Program _ stmts) = do
    let builtinsDefs = map (uncurry DefFc) builtins
    evalBlock Global (builtinsDefs ++ stmts)


builtins :: [(Id, Func)]
builtins =
    [ ( "WriteLn"
      , Func [("x", IntType)]
             [IOInteropStmt Print [VarExpression "x"]]
             "z"
             NullType
      )
    , ( "ReadLn"
      , Func [] [IOInteropStmt Read [VarExpression "ret"]] "ret" IntType
      )
    ]

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
evalNumOp _ Null          _ = raiseError "Invalid operation"

evalUnaryOp op (IntValue this) = case op of
    Neg -> return $ IntValue $ negate this
    Pos -> return $ IntValue this
    _   -> raiseError "Invalid operation"



-- Bool definitions of operations
evalUnaryOp _ (BoolValue _) = raiseError "Invalid operation"
evalUnaryOp _ Null          = raiseError "Invalid operation"


evalRelOp op (IntValue this) other = case other of
    IntValue o -> case op of
        Lt  -> return $ BoolValue $ this < o
        Le  -> return $ BoolValue $ this <= o
        Gt  -> return $ BoolValue $ this > o
        Ge  -> return $ BoolValue $ this >= o
        Eq  -> return $ BoolValue $ this == o
        Neq -> return $ BoolValue $ this /= o
    _ -> raiseError "Invalid operation"

evalRelOp op (BoolValue this) other = case other of
    BoolValue o -> case op of
        Eq  -> return $ BoolValue $ this == o
        Neq -> return $ BoolValue $ this /= o
        _   -> raiseError "Invalid operation"
    _ -> raiseError "Invalid operation"

evalRelOp op Null other = case other of -- Null is only equal to Null
    Null -> case op of
        Eq  -> return $ BoolValue True
        Neq -> return $ BoolValue False
        _   -> raiseError "Invalid operation"
    _ -> raiseError "Invalid operation"

