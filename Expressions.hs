module Expressions where

data RelOp = Eq | Neq deriving Show

data NumOp = Add | Mul deriving Show

data UnaryOp = Neg | Not | Pos deriving Show

data ValueType = IntType Int | BoolType Bool deriving Show

data Expression =
    FunctionCall {
    id :: String,
    args :: [Expression]
    }
    | Var {name :: String}
    | NumExpression {numOp :: NumOp, left :: Expression, right :: Expression}
    | RelExpression {relOp :: RelOp, left :: Expression, right :: Expression}
    | ValueType {value :: Int}
    | UnaryExpression {op :: UnaryOp, right :: Expression}
    deriving Show



-- | Proměnná je řetězec
type Var = String                -- <var>

-- | Příkazy samy o sobě nemají hodnotu, ale manipulují s proměnnými
data Statement = Assign Var Expression        -- <var> = <expr>
    deriving Show

-- | Skupina příkazů je blok
type Block = [Statement]

data If = If Expression Bool Block Block
