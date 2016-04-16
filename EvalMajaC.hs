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
 
type Loc = Int
type Var = Ident
type Store = (M.Map Loc ExpResult, Loc)
type VEnv = M.Map Var Loc
--type FEnv = M.Map Var ()

data S = S (VEnv, Store) deriving (Eq, Ord, Show)  

initialStore :: Store
initialStore = (M.empty, 0)

initialVEnv :: VEnv
initialVEnv = M.empty

initS :: S
initS = S (initialVEnv, initialStore)

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
execProg (Prog p) = execProg' p initS 
            where
               execProg' [] (S(e,st)) = mapM_ (putStrLn . show) $ M.toList $ e
               execProg' (p:ps) (S(e,st)) = execProg' ps $ execStmt p (S(e,st))
                                          
majaBOp :: (Bool -> Bool -> Bool) -> ExpResult -> ExpResult -> ExpResult
majaBOp f (MajaBool b1) (MajaBool b2) = MajaBool $ f b1 b2

majaDiv :: ExpResult -> ExpResult -> ExpResult
(majaDiv) (MajaInt i1) (MajaInt i2) = MajaInt $ i1 `div` i2

majaNot :: ExpResult -> ExpResult
majaNot (MajaBool b) = MajaBool $ not b

getLoc :: MonadState (S) m => Var -> m Loc
getLoc v = do
            S (venv, _) <- get
            --x <- M.lookup v venv
            return $ fromMaybe (error "<getLoc>: Undefined variable") $ M.lookup v venv 

getVar :: MonadState (S) m => Var -> m ExpResult
getVar v = do
             loc <- getLoc v
             S(_, (store, _)) <- get
             return $ fromMaybe (error "<getVar>: Undefined variable") $ M.lookup loc store 

assign :: MonadState (S) m => ExpResult -> Loc -> m ()
assign e loc = do
               S (venv, (store, l)) <- get
               case M.lookup loc store of
                  Just expr -> put $ S (venv, (M.insert loc e store, l))
                  Nothing -> error ("Location unused")

evalExp e s = execState (evalExpM e) s

evalExpM :: (MonadState (S) m) => Exp -> m ExpResult
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

execStmtM :: (MonadState (S) m) => Stmt -> m ()
execStmtM (SAssign v e) = do
                           x <- evalExpM e
                           loc <- getLoc v
                           assign x loc
{-execStmtM (SAssignS v vf e) = do
                                 x <- evalExpM e
                                 s <- getStruc-} 
execStmtM (SBlock (SBl b)) = execStmtM' b
                        where
                           execStmtM' [] = return ()
                           execStmtM' (b:bs) = execStmtM b >> execStmtM' bs 
