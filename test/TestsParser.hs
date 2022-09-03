module Main where
import           Expressions
import           Parser
import           Test.Hspec

parserExpressions = do
    it "should parse add expression" $ do
        parse parseExpression "1+2" `shouldBe` Right
            (NumExpression Add
                           (ValueExpression (IntValue 1))
                           (ValueExpression (IntValue 2))
            )

    it "should parse mul expression" $ do
        parse parseExpression "1*2" `shouldBe` Right
            (NumExpression Mul
                           (ValueExpression (IntValue 1))
                           (ValueExpression (IntValue 2))
            )

    it "should have correct precedence mul/add" $ do
        parse parseExpression "1+2*3" `shouldBe` Right
            (NumExpression
                Add
                (ValueExpression (IntValue 1))
                (NumExpression Mul
                               (ValueExpression (IntValue 2))
                               (ValueExpression (IntValue 3))
                )
            )

    it "should have correct precedence left precedence" $ do
        parse parseExpression "1-2+3" `shouldBe` Right
            (NumExpression
                Add
                (NumExpression Sub
                               (ValueExpression (IntValue 1))
                               (ValueExpression (IntValue 2))
                )
                (ValueExpression (IntValue 3))
            )

    it "should parse factors" $ do
        parse parseExpression "x*(1--2)*-1" `shouldBe` Right
            (NumExpression
                Mul
                (NumExpression
                    Mul
                    (VarExpression "x")
                    (NumExpression
                        Sub
                        (ValueExpression (IntValue 1))
                        (UnaryExpression Neg (ValueExpression (IntValue 2)))
                    )
                )
                (UnaryExpression Neg (ValueExpression (IntValue 1)))
            )
    it "should parse function call" $ do
        parse parseExpression "fx(1,x+1)" `shouldBe` Right
            (FunctionCallExpression
                (FunctionCall
                    "fx"
                    [ ValueExpression (IntValue 1)
                    , NumExpression Add
                                    (VarExpression "x")
                                    (ValueExpression (IntValue 1))
                    ]
                )
            )
    it "should parse empty function call" $ do
        parse parseExpression "fx()"
            `shouldBe` Right (FunctionCallExpression (FunctionCall "fx" []))



parserStatements = do
    it "should parse assignment" $ do
        parse parseStatement "x:=1"
            `shouldBe` Right (Assign "x" (ValueExpression (IntValue 1)))

    it "should parse function call" $ do
        parse parseStatement "fx(1,x+1)" `shouldBe` Right
            (FunctionCallStmt
                (FunctionCall
                    "fx"
                    [ ValueExpression (IntValue 1)
                    , NumExpression Add
                                    (VarExpression "x")
                                    (ValueExpression (IntValue 1))
                    ]
                )
            )
    it "should parse if statement" $ do
        parse parseIfStatement "if (x==1) then x:=2 else begin x:=3; end"
            `shouldBe` Right
                           (If
                               (RelExpression Eq
                                              (VarExpression "x")
                                              (ValueExpression (IntValue 1))
                               )
                               [Assign "x" (ValueExpression (IntValue 2))]
                               [ CompoundStmt
                                     [Assign "x" (ValueExpression (IntValue 3))]
                               ]
                           )
    it "should parse while statement" $ do
        parse parseStatement "while x==1 do x:=2" `shouldBe` Right
            (While
                (RelExpression Eq
                               (VarExpression "x")
                               (ValueExpression (IntValue 1))
                )
                [Assign "x" (ValueExpression (IntValue 2))]
            )

parserFunction = do
    it "should parse var declaration" $ do
        parse parseIdListWithType "x,y : integer"
            `shouldBe` Right [("x", IntType), ("y", IntType)]


    it "should parse function" $ do
        parse parseFunctionDeclaration
              "function f(x,y : integer) : integer;\nbegin\nf:=x+y;\nend"
            `shouldBe` Right
                           (DefFc
                               "f"
                               (Func
                                   [("x", IntType), ("y", IntType)]
                                   [ Assign
                                         "f"
                                         (NumExpression Add
                                                        (VarExpression "x")
                                                        (VarExpression "y")
                                         )
                                   ]
                                   "f"
                                   IntType
                               )
                           )
    it "should run complex function"
        $          do
                       parse
                           parseFunctionDeclaration
                           "function max(num1, num2: integer): integer;\n \
                \var\n \
                \result: integer;\n \
                \\n\
                \begin\n\
                \if (num1 == num2) then\n\
                \    result := num1\n\
                \else\
                \    result := num2;\
                \max := result;\
                \end"
        `shouldBe` Right
                       (DefFc
                           "max"
                           (Func
                               [("num1", IntType), ("num2", IntType)]
                               [ DefVar "result" IntType
                               , If
                                   (RelExpression Eq
                                                  (VarExpression "num1")
                                                  (VarExpression "num2")
                                   )
                                   [Assign "result" (VarExpression "num1")]
                                   [Assign "result" (VarExpression "num2")]
                               , Assign "max" (VarExpression "result")
                               ]
                               "max"
                               IntType
                           )
                       )



    it "should parse program" $ do
        parse parseProgram
              "program p;\nvar x,y : integer;\nbegin\nx:=1;\ny:=2;\nend."
            `shouldBe` Right
                           (Program
                               "p"
                               [ DefVar "x" IntType
                               , DefVar "y" IntType
                               , Assign "x" (ValueExpression (IntValue 1))
                               , Assign "y" (ValueExpression (IntValue 2))
                               ]
                           )





expressionSpec = describe "expressionSpec" $ do
    parserExpressions

stmtSpec = describe "statementSpec" $ do
    parserStatements

functionSpec = describe "functionSpec" $ do
    parserFunction

main :: IO ()
main = hspec $ do
    expressionSpec
    stmtSpec
    functionSpec
