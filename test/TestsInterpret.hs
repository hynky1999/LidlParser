module Main where

import           Test.Hspec

import qualified Data.Map                      as Map
import           Expressions
import           Interpreter


num :: Int -> Expression
num x = Expressions.ValueExpression (Expressions.IntValue x)

add :: Expression -> Expression -> Expression
add = NumExpression Add

mul :: Expression -> Expression -> Expression
mul = NumExpression Mul


emptyStack = Interpreter.Store Map.empty Map.empty

interpretExpressionTests = do
    it "should interpret simple expression" $ do
        let expr = add (num 1) (mul (num 2) (num 3))
        interpret (evalExpr expr) emptyStack `shouldBe` Right (IntValue 7)

    it "should interpret simple expression with variables" $ do
        let expr = add (add (VarExpression "x") (num 3)) (VarExpression "y")
        interpret
                (evalExpr expr)
                (Interpreter.Store
                    (Map.fromList [("x", IntValue 2), ("y", IntValue 3)])
                    Map.empty
                )
            `shouldBe` Right (IntValue 8)

    it "should interpret relational operand" $ do
        let expr = RelExpression Eq (num 1) (num 2)
        interpret (evalExpr expr) emptyStack `shouldBe` Right (BoolValue False)
        let expr = RelExpression Eq (num 1) (num 1)
        interpret (evalExpr expr) emptyStack `shouldBe` Right (BoolValue True)

    it "should interpret unary operands" $ do
        let expr = UnaryExpression Neg (num 1)
        interpret (evalExpr expr) emptyStack `shouldBe` Right (IntValue (-1))
        let expr = UnaryExpression Neg (num (-1))
        interpret (evalExpr expr) emptyStack `shouldBe` Right (IntValue 1)


    it "should not work bool + num" $ do
        let expr = add (ValueExpression (BoolValue True)) (num 1)
        interpret (evalExpr expr) emptyStack `shouldBe` Left "Invalid operation"


interpretStatementTests = do
    it "should interpret simple assign" $ do
        let block = [Define "x" IntType, Assign "x" (num 1)]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 1)]

    it "should return error on typecheck" $ do
        let
            block =
                [ Define "x" IntType
                , Assign "x" (ValueExpression (BoolValue True))
                ]
        interpret (evalBlock block) emptyStack
            `shouldBe` Left "Cannot assign BoolType to IntType"

    it "should interpret simple if success" $ do
        let
            block =
                [ Define "x" IntType
                , Assign "x" (num 1)
                , If (RelExpression Eq (VarExpression "x") (num 1))
                     [Assign "x" (num 2)]
                     []
                ]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 2)]

    it "should interpret simple if fail" $ do
        let
            block =
                [ Define "x" IntType
                , Assign "x" (num 3)
                , If (RelExpression Eq (VarExpression "x") (num 1))
                     [Assign "x" (num 2)]
                     []
                ]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 3)]

    it "should interpret simple while" $ do
        let
            block =
                [ Define "x" IntType
                , Assign "x" (num 1)
                , While (RelExpression Eq (VarExpression "x") (num 1))
                        [Assign "x" (num 2)]
                ]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 2)]

interpretFunctionTests = do
    it "should interpret simple function" $ do
        let params = []
        let block  = [Assign "ret" (num 10)]
        let fc     = Func params block "ret" IntType
        let block =
                [ FunctionDef "f" fc
                , Define "x" IntType
                , Assign "x" (FunctionCallExpression (FunctionCall "f" []))
                ]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 10)]

    it "should interpret simple funtion with args" $ do
        let params = [("x", IntType), ("y", IntType)]
        let block =
                [Assign "ret" (add (VarExpression "x") (VarExpression "y"))]
        let fc = Func params block "ret" IntType
        let
            block =
                [ FunctionDef "f" fc
                , Define "x" IntType
                , Assign
                    "x"
                    (FunctionCallExpression (FunctionCall "f" [num 1, num 2]))
                ]
        storeVars (interpretDebug (evalBlock block) emptyStack)
            `shouldBe` Map.fromList [("x", IntValue 3)]




expSpec = describe "expSpec" $ do
    interpretExpressionTests

stmtSpec = describe "stmtSpec" $ do
    interpretStatementTests

fcsSpec = describe "fcsSpec" $ do
    interpretFunctionTests

main :: IO ()
main = hspec $ do
    expSpec
    interpretStatementTests
    interpretFunctionTests


