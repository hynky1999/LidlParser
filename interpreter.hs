-- Tato možnost zapne všechny warningy:
{-# OPTIONS_GHC -Wall #-}

module Interpreter where

import qualified Data.Map                      as Map

import           State


-- | Výrazy mají hodnotu samy o sobě
data Expression                       -- <expr>
  = ValueType                        -- 42
  | Variable Var                      -- <var>
  | BinaryOp Op Expression Expression -- <expr> <op> <expr>
  deriving (Eq, Show)


data If = If Expression Bool Block Block

-- | Reprezentuje druh binární operace
data Op                             -- <expr>
  = Add                             -- (+)
  | Sub                             -- (-)
  | Mul                             -- (*)
  | Div                             -- (/)
  deriving (Eq, Show)

-- | Hodnoty jsou pouze čísla.
-- | Vlastní pseudo typy abychom si mohli zadefinovat vlastní instance Num
data ValueType = IntType Int | BoolType  Bool | StringType String

-- | 'Store' je mapa, která proměnným přiřazuje hodnoty.
type RuntimeError = String


type Store = Map.Map Var Either ValueType RuntimeError

{- Příkladný program si můžete představit například následovně:
> x = 4 + 5 + 3
> y = (x * 42) + 1
> z = x + y
> x += z
> result = (x * x * x * x) / 4
-}

-- | Vyhodnocení výrazu probíhá následovně:
--
-- Pokud je výraz číslo, tak jej vrátí.
-- Pokud je výraz proměnná, tak se podívá do 'Store'.
--   Pokud má proměnná přiřazenou hodnotu, pak ji vrátí.
--   Pokud proměnná nemá přiřazenou hodnotu, vrátí nulu.
-- Pokud je výraz nějaká binární operace, zavolá 'evalBinaryOp'
--
-- Pokud se výraz skládá z více částí, je nejprve vyhodnocena levá část a až potom pravá část
evalExpr :: Expression -> State Store Value
evalExpr expr = case expr of
    Number   n -> return n
    String   s -> return s
    Variable v -> do
        store <- get
        return findWithDefault 0 v store
    BinaryOp op l r -> evalBinaryOp op l r

-- | Vyhodnocení binární operace

evalBinaryOp :: Op -> Expression -> Expression -> State Store Value
evalBinaryOp = apply . getOp
  where
  -- | Převede 'Op' na funkci, která vezme dvě hodnoty a vrátí novou hodnotu
    getOp :: Op -> (Value -> Value -> Value)
    getOp op = case op of
        Add   -> (+)
        Sub   -> (-)
        Mul   -> (*)
        Div   -> div
        Eq    -> \x y -> if x == y then 1 else 0
        NonEq -> \x y -> if getOp Eq x y == 0 then 1 else 0

    -- | Vezme funkci získanou z 'getOp' výše,
    -- dva výrazy, které vyhodnotí pomocí 'evalExpr'
    -- a aplikuje na ně kombinující funkci.
    --
    -- Pokuste se o co nejelegantnější zápis.
    --
    -- Hint: Tady budete potřebovat 'do'-notaci.
    apply
        :: (Value -> Value -> Value)
        -> Expression
        -> Expression
        -> State Store Value
    apply opFn e1 e2 = do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        return $ opFn v1 v2


evalIf :: If -> State Store Value
-- | Vyhodnocení příkazu
--
-- Přiřazení (Assign) je vyhodnoceno tak, že je vyhodnocen výraz na pravé straně
-- a potom je výsledná hodnota přiřazena do proměnné (vložena do 'Store').
--
-- Přiřazení s operací (AssignOp) je vyhodnoceno podobně jako výše uvedené přiřazení.
-- Využijte funkci 'evalBinaryOp', abyste si ulehčili práci.
-- [Nápověda: x += y je to samé jako x = (x + y)]
evalStatement :: Statement -> State Store ()
evalStatement stmt = case stmt of
    Assign v e -> do
        v' <- evalExpr e
        modify (Map.insert v v')
    AssignOp op v e -> do
        v' <- evalExpr e
        modify (Map.insert v (evalBinaryOp op (Variable v) (Number v')))

-- | Vyhodnocení bloku příkazů probíhá tak,
-- že je postupně vyhodnocen každý příkaz
evalBlock :: Block -> State Store ()
evalBlock = mapM_ evalStatement

-- | Celkové vyhodnocení znamená spustit stavový výpočet 'evalBlock' s prázdným Storem
eval :: Block -> Store
eval = evalState Map.empty . evalBlock

-- Pokud chcete bonusové body:
--
-- * Vyrobte sadu testů, která pokryje většinu situací, které mohou nastat
-- * Přidejte `==`, které vrací 0/1, `if` příkaz [(If Expr Block Block)] a napište pro něj vyhodnocení
-- * Přidejte hezkou `Show` instanci, která vše hezky vypíše
-- * Napište parser pomocí monády `Parser` ze cvičení!


