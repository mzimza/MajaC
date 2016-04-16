--Author Maja Zalewska
--Index 336088
--Evaluation file
--

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module EvalMajaC where

import AbsMajaC
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe


data ExpResult = MajaInt Integer | MajaBool Bool
 
type Var = Ident
type Store = M.Map Var ExpResult
type VEnv = M.Map Var ExpResult
--type FEnv = M.Map Var ()

initialStore :: Store
initialStore = M.empty

instance Eq ExpResult where
   (MajaInt i1) == (MajaInt i2) = i1 == i2
   (MajaBool b1) == (MajaBool b2) = b1 == b2
   _ == _ = False 

instance Ord ExpResult where
   (MajaInt i1) <= (MajaInt i2) = i1 <= i2
   (MajaBool b1) <= (MajaBool b2) = b1 <= b2
   _ <= _ = False

instance Num ExpResult where
   (MajaInt i1) + (MajaInt i2) = MajaInt (i1 + i2)
   (MajaInt i1) - (MajaInt i2) = MajaInt (i1 - i2)
   (MajaInt i1) * (MajaInt i2) = MajaInt (i1 * i2)
   abs (MajaInt i1) = MajaInt (abs i1)
   fromInteger i = MajaInt i
   signum (MajaInt i1) = MajaInt (signum i1)

instance Show ExpResult where
   show (MajaInt i) = show i
   show (MajaBool b) = show b

execProg :: Program -> IO ()
execProg (Prog p) = execProg' p initialStore 
            where
               execProg' [] s = mapM_ (putStrLn . show) $ M.toList s
               execProg' (p:ps) s = execProg' ps $ execStmt p s
                                          
majaBOp :: (Bool -> Bool -> Bool) -> ExpResult -> ExpResult -> ExpResult
majaBOp f (MajaBool b1) (MajaBool b2) = MajaBool $ f b1 b2

majaDiv :: ExpResult -> ExpResult -> ExpResult
(majaDiv) (MajaInt i1) (MajaInt i2) = MajaInt $ i1 `div` i2

majaNot :: ExpResult -> ExpResult
majaNot (MajaBool b) = MajaBool $ not b

getVar :: MonadState (Store) m => Var -> m ExpResult
getVar v = do
             x <- gets (M.lookup v)
             return $ fromMaybe (error "Undefined variable") x

evalExp e = execState (evalExpM e) M.empty

evalExpM :: (MonadState (Store) m) => Exp -> m ExpResult
evalExpM (EOr e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ majaBOp (||) x y

evalExpM (EAnd e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ majaBOp (&&) x y

evalExpM (EEq e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x == y

evalExpM (ENeq e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x /= y
                           
evalExpM (ELt e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x < y

evalExpM (EGt e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x > y

evalExpM (ELe e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x <= y

evalExpM (EGe e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ MajaBool $ x >= y

evalExpM (EAdd e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ x + y

evalExpM (ESub e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ x - y

evalExpM (EMul e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           return $ x * y

evalExpM (EDiv e1 e2) = do
                           x <- evalExpM e1
                           y <- evalExpM e2
                           if y == (MajaInt 0) then error "Division by 0!"
                           else return $ x `majaDiv` y

evalExpM (EPreop Negative e) = do
                                 x <- evalExpM e
                                 return $ MajaInt (-1) * x

evalExpM (EPreop LogNeg e) = do
                                x <- evalExpM e
                                return $ majaNot x

--tu te, ktorych nie ma

evalExpM (EVar v) = getVar v 

evalExpM (EConst CTrue) = return $ MajaBool True
evalExpM (EConst CTrueB) = return $ MajaBool True
evalExpM (EConst CFalse) = return $ MajaBool False
evalExpM (EConst CFalseB) = return $ MajaBool False
evalExpM (EConst (CInt i)) = return $ MajaInt i 

--STATEMENTS

execStmt s m = execState (execStmtM s) m

execStmtM :: (MonadState (Store) m) => Stmt -> m ()
execStmtM (SAssign v e) = evalExpM e >>= modify . M.insert v
{-execStmtM (SAssignS v vf e) = do
                                 x <- evalExpM e
                                 s <- getStruc-} 
execStmtM (SBlock (SBl b)) = execStmtM' b
                        where
                           execStmtM' [] = return ()
                           execStmtM' (b:bs) = execStmtM b >> execStmtM' bs 
