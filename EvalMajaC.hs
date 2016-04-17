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
               execProg' (p:ps) (S(e,st)) = do
                                             x <- execStmt p (S(e, st))
                                             execProg' ps x 
                                          
majaBOp :: (Bool -> Bool -> Bool) -> ExpResult -> ExpResult -> ExpResult
majaBOp f (MajaBool b1) (MajaBool b2) = MajaBool $ f b1 b2

majaDiv :: ExpResult -> ExpResult -> ExpResult
(majaDiv) (MajaInt i1) (MajaInt i2) = MajaInt $ i1 `div` i2

majaNot :: ExpResult -> ExpResult
majaNot (MajaBool b) = MajaBool $ not b

getLoc :: MonadState S m => Var -> m Loc
getLoc v = do
            S (venv, _) <- get
            return $ fromMaybe (error "<getLoc>: Undefined variable") $ M.lookup v venv 

getVar :: MonadState S m => Var -> m ExpResult
getVar v = do
             loc <- getLoc v
             S(_, (store, _)) <- get
             return $ fromMaybe (error "<getVar>: Undefined variable") $ M.lookup loc store 

assign :: MonadState S m => ExpResult -> Loc -> m ()
assign e loc = do
               S (venv, (store, l)) <- get
               case M.lookup loc store of
                  Just expr -> put $ S (venv, (M.insert loc e store, l))
                  Nothing -> error ("Location unused")

newVar :: MonadState S m => Type -> Var -> m ()
newVar t v = do
            S(venv, (store, loc)) <- get
            case t of
              TInt -> put $ S ((M.insert v loc venv), (M.insert loc (MajaInt 0) store, (loc + 1)))
              TBool -> put $ S ((M.insert v loc venv), (M.insert loc (MajaBool False) store, (loc + 1)))

newVarAssign :: MonadState S m => Type -> Var -> ExpResult -> m ()
newVarAssign t v e = do
            S(venv, (store, loc)) <- get
            case t of
              TInt -> put $ S ((M.insert v loc venv), (M.insert loc e store, (loc + 1)))
              TBool -> put $ S ((M.insert v loc venv), (M.insert loc e store, (loc + 1)))

localEnv :: MonadState S m => m t -> m ()
localEnv f = do
               S (venv, store) <- get
               exec <- f
               S (_, store') <- get
               put $ S (venv, store')

ifelse :: ExpResult -> t -> t -> t
ifelse e f1 f2 = case e of
                     MajaBool True -> f1
                     MajaBool False -> f2

evalExp e s = execStateT (evalExpM e) s

evalExpM :: (MonadTrans m, MonadState S (m IO)) => Exp -> m IO ExpResult
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

execStmt s m = execStateT (execStmtM s) m

execStmtB :: (MonadTrans m, MonadState S (m IO)) => Block -> m IO ()
execStmtB (SBl b) = localEnv $ execStmtM' b
                        where
                           execStmtM' [] = return ()
                           execStmtM' (s:ss) = execStmtM s >> execStmtM' ss

execStmtM :: (MonadTrans m, MonadState S (m IO)) => Stmt -> m IO ()
execStmtM (SAssign v e) = do
                           x <- evalExpM e
                           loc <- getLoc v
                           assign x loc
{-execStmtM (SAssignS v vf e) = do
                                 x <- evalExpM e
                                 s <- getStruc-} 
execStmtM (SBlock b) = execStmtB b

--Decl/DeclArr/DeclVar
execStmtM (SDeclV (DVar t v) e) = evalExpM e >>= newVarAssign t v

execStmtM (SIf e b) = do
                        x <- evalExpM e
                        ifelse x (execStmtB b) (return ())
execStmtM (SIfElse e b1 b2) = do
                              x <- evalExpM e
                              ifelse x (execStmtB b1) (execStmtB b2)
--execStmtM (SFor d e1 e2 s b) = do

execStmtM while@(SWhile e b) = do
                           x <- evalExpM e
                           ifelse x 
                                  (execStmtB b >> execStmtM while)
                                  (return ())

execStmtM (SPrint e) = do
                        x <- evalExpM e
                        lift $  putStrLn $ show x

execDeclM :: (MonadTrans m, MonadState S (m IO)) => Decl -> m IO ()
execDeclM (DeclV (DVar t v)) = newVar t v

--pozostale deklaracje, jak bede miec typy
--execDeclM (DeclF
