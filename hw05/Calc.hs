{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import StackVM
import qualified Data.Map as M
import qualified ExprT as VarExprT

-- Ex 1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- Ex 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
                  Just expr -> Just (eval expr)
                  _ -> Nothing

-- Ex 3
class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

instance Expr ExprT where
   lit = ExprT.Lit
   add = ExprT.Add
   mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0 <)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax l1) (MinMax l2) = MinMax (max l1 l2)
    mul (MinMax l1) (MinMax l2) = MinMax (min l1 l2)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 l1) (Mod7 l2) = Mod7 (mod (l1 + l2) 7)
    mul (Mod7 l1) (Mod7 l2) = Mod7 (mod (l1 * l2) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5
-- exp :: Expr a => a

-- stackVM exp == Right [IVal exp]

instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
    mul p1 p2= p1 ++ p2 ++ [StackVM.Mul]

testProg = testExp :: Maybe StackVM.Program

justMinusSeven = stackVM <$> testProg

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

run s = case stackVM <$> parseExp lit add mul s
                of Just (Right v) -> v
                   Just (Left e) -> error e
                   _ -> error "not value"

-- Exercise 6
class HasVars a where
    var :: String -> a

-- data VarExprT = VLit Integer 
--     | VAdd VarExprT VarExprT
--     | VMul VarExprT VarExprT
--     | VVar String
--         deriving (Show, Eq)

-- instance Expr VarExprT where
--     lit = VLit
--     mul = VMul
--     add = VAdd

-- instance HasVars VarExprT where
--   var = VVar 

type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    add e1 e2 m = (+) <$> e1 m <*> e2 m
    mul e1 e2 m = (*) <$> e1 m <*> e2 m
    lit x _ = Just x

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- evalWithvars :: [(String, Integer)]
--     -> Expr -> Maybe Integer