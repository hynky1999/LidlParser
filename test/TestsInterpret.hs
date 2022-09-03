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

storeVarsGet :: Scope -> Either RuntimeError Store -> Maybe (Map.Map Id Value)
storeVarsGet scope store = case store of
    Left  _ -> Nothing
    Right s -> Just (vars (storeToStack scope s))




interpretExpressionTests = do
    it "should interpret simple expression" $ do
        let expr = add (num 1) (mul (num 2) (num 3))
        res <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res `shouldBe` Right (IntValue 7)

    it "should interpret simple expression with variables" $ do
        let expr = add (add (VarExpression "x") (num 3)) (VarExpression "y")
        res <- interpret
            (evalExpr expr)
            (Interpreter.Store
                (Stack (Map.fromList [("x", IntValue 2), ("y", IntValue 3)])
                       Map.empty
                )
                emptyStack
            )
        res `shouldBe` Right (IntValue 8)

    it "should interpret relational operand" $ do
        let expr = RelExpression Eq (num 1) (num 2)
        res1 <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res1 `shouldBe` Right (BoolValue False)
        let expr = RelExpression Eq (num 1) (num 1)
        res2 <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res2 `shouldBe` Right (BoolValue True)

    it "should interpret unary operands" $ do
        let expr = UnaryExpression Neg (num 1)
        res1 <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res1 `shouldBe` Right (IntValue (-1))
        let expr = UnaryExpression Neg (num (-1))
        res2 <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res2 `shouldBe` Right (IntValue 1)


    it "should not work bool + num" $ do
        let expr = add (ValueExpression (BoolValue True)) (num 1)
        res <- interpret (evalExpr expr) (Store emptyStack emptyStack)
        res `shouldBe` Left "Invalid operation"


interpretStatementTests = do
    it "should interpret simple assign globalStack" $ do
        let block = [DefVar "x" IntType, Assign "x" (num 1)]
        res <- interpretDebug (evalBlock Global block)
                              (Store emptyStack emptyStack)
        res
            `shouldBe` Right
                           (Store
                               (Stack (Map.fromList [("x", IntValue 1)])
                                      Map.empty
                               )
                               emptyStack
                           )

    it "should interpret simple assign localStack" $ do
        let block = [DefVar "x" IntType, Assign "x" (num 1)]
        res <- interpretDebug (evalBlock Local block)
                              (Store emptyStack emptyStack)
        res
            `shouldBe` Right
                           (Store
                               emptyStack
                               (Stack (Map.fromList [("x", IntValue 1)])
                                      Map.empty
                               )
                           )

    it "should return error on typecheck" $ do
        let
            block =
                [ DefVar "x" IntType
                , Assign "x" (ValueExpression (BoolValue True))
                ]
        res <- interpret (evalBlock Local block) (Store emptyStack emptyStack)
        res `shouldBe` Left "Cannot assign BoolType to IntType"

    it "should interpret simple if success" $ do
        let
            block =
                [ DefVar "x" IntType
                , Assign "x" (num 1)
                , If (RelExpression Eq (VarExpression "x") (num 1))
                     [Assign "x" (num 2)]
                     []
                ]

        res <- interpretDebug (evalBlock Global block)
                              (Store emptyStack emptyStack)
        res
            `shouldBe` Right
                           (Store
                               (Stack (Map.fromList [("x", IntValue 2)])
                                      Map.empty
                               )
                               emptyStack
                           )

    it "should interpret simple if fail" $ do
        let
            block =
                [ DefVar "x" IntType
                , Assign "x" (num 3)
                , If (RelExpression Eq (VarExpression "x") (num 1))
                     [Assign "x" (num 2)]
                     []
                ]
        res <- interpretDebug (evalBlock Global block) emptyStore
        res
            `shouldBe` Right
                           (Store
                               (Stack (Map.fromList [("x", IntValue 3)])
                                      Map.empty
                               )
                               emptyStack
                           )

    it "should interpret simple while" $ do
        let
            block =
                [ DefVar "x" IntType
                , Assign "x" (num 1)
                , While (RelExpression Eq (VarExpression "x") (num 1))
                        [Assign "x" (num 2)]
                ]
        res <- interpretDebug (evalBlock Global block) emptyStore
        res
            `shouldBe` Right
                           (Store
                               (Stack (Map.fromList [("x", IntValue 2)])
                                      Map.empty
                               )
                               emptyStack
                           )

interpretFunctionTests = do
    it "should interpret simple function" $ do
        let params = []
        let block  = [Assign "ret" (num 10)]
        let fc     = Func params block "ret" IntType
        let block =
                [ DefFc "f" fc
                , DefVar "x" IntType
                , Assign "x" (FunctionCallExpression (FunctionCall "f" []))
                ]
        res <- interpretDebug (evalBlock Global block) emptyStore
        storeVarsGet Global res
            `shouldBe` Just (Map.fromList [("x", IntValue 10)])

    it "should interpret simple funtion with args" $ do
        let params = [("x", IntType), ("y", IntType)]
        let block =
                [Assign "ret" (add (VarExpression "x") (VarExpression "y"))]
        let fc = Func params block "ret" IntType
        let
            block =
                [ DefFc "f" fc
                , DefVar "x" IntType
                , Assign
                    "x"
                    (FunctionCallExpression (FunctionCall "f" [num 1, num 2]))
                ]
        res <- interpretDebug (evalBlock Global block) emptyStore
        storeVarsGet Global res
            `shouldBe` Just (Map.fromList [("x", IntValue 3)])

    it "should fail becaue stack was emptied" $ do
        let params = [("x", IntType), ("y", IntType)]
        let block =
                [Assign "ret" (add (VarExpression "x") (VarExpression "y"))]
        let fc = Func params block "ret" IntType
        let block =
                [ DefFc "f" fc
                , DefVar "x" IntType
                , FunctionCallStmt (FunctionCall "f" [num 1, num 2])
                , Assign "x" (VarExpression "y")
                ]
        res <- interpretDebug (evalBlock Global block) emptyStore
        res `shouldBe` Left "Variable y not found"





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


