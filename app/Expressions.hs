module Expressions where

{-# LANGUAGE DeriveDataTypeable #-}

data RelOp = Eq | Neq deriving (Show, Eq)

data NumOp = Add | Sub | Mul | Div deriving (Show, Eq)

data UnaryOp = Neg | Not | Pos deriving (Show, Eq)


data Value = IntValue Int | BoolValue Bool | Null deriving (Show, Eq)

typeOf :: Value -> Type
typeOf (IntValue  _) = IntType
typeOf (BoolValue _) = BoolType
typeOf Null          = NullType

defaultValue :: Type -> Value
defaultValue IntType  = IntValue 0
defaultValue BoolType = BoolValue False
defaultValue NullType = Null

data Type = IntType | BoolType | NullType deriving (Show, Eq)


data FunctionCall = FunctionCall
    { functionCallName :: String
    , functionCallArgs :: [Expression]
    }
    deriving (Show, Eq)

data Expression =
    FunctionCallExpression FunctionCall
    | VarExpression Id
    | NumExpression {numOp :: NumOp, left :: Expression, right :: Expression}
    | RelExpression {relOp :: RelOp, left :: Expression, right :: Expression}
    | ValueExpression {value :: Value}
    | UnaryExpression {unaryOp :: UnaryOp, right :: Expression}
    deriving (Show, Eq)



type Id = String

data Func = Func
    { functionArgs     :: [(Id, Type)]
    , functionBlock    :: Block
    , functionRetName  :: Id
    , functionRetValue :: Type
    }

    deriving (Show, Eq)

data BuiltInFunction = Print
    deriving (Show, Eq)


data Scope = Global | Local deriving (Show, Eq)

data Statement = Assign Id Expression        -- <var> = <expr>
                | DefVar Id Type -- type <var>
                | DefFc Id Func -- def <id> <func>
                | FunctionCallStmt FunctionCall -- f(x)
                | If Expression Block Block -- if <expr> <block> else <block>
                | While Expression Block -- while <expr> <block>
    deriving (Show, Eq)

-- | Skupina příkazů je blok
type Block = [Statement]
data Program = Program Id Block
    deriving (Show, Eq)
