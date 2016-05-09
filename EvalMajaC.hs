--Author: Maja Zalewska
--index nr: 336088
--Evaluation file for MajaC programming language
--
{-# LANGUAGE TypeSynonymInstances #-}                                              
{-# LANGUAGE FlexibleInstances #-}                                                 
{-# LANGUAGE FlexibleContexts #-}                                                  
                                                                                   
module EvalMajaC2 where                                                             
                                                                                   
import AbsMajaC                                                                    
import Control.Monad.Reader                                                        
import Control.Monad.State                                                         
import qualified Data.Map as M                                                     
import Data.Maybe                                                                  
import Data.List                                                                                   

data ExpResult = MajaInt Integer | MajaBool Bool | MajaVoid  
               | MajaArray [Loc] | MajaArrayEval [ExpResult]  
               | MajaTuple [Loc] | MajaTupleEval [ExpResult] 
               | MajaStruct Var (M.Map Var Loc) | MajaStructEval Var (M.Map Var ExpResult)
                                                                                   
type Loc = Int                                                                     
type Var = Ident                                                                   
data  StructElem = SBasic Type | SArr Type Int deriving (Eq, Ord, Show) 
type Store = (M.Map Loc ExpResult, Loc)                                            
type VEnv = M.Map Var Loc                                                          
type FEnv = M.Map Var Func                                                         
type SEnv = M.Map Var (M.Map Var StructElem)                                                 
data Func = Func ([DeclVar], [Stmt], Exp, Type, FEnv, VEnv, SEnv) deriving (Eq, Ord, Show)
                                                                                   
data S = S (FEnv, VEnv, SEnv, Store) deriving (Eq, Ord)                            
                                                                                   
initialStore :: Store                                                              
initialStore = (M.empty, 0) 
                                                                                   
initialVEnv :: VEnv                                                                
initialVEnv = M.empty
                                                                                   
initialFEnv :: FEnv                                                                
initialFEnv = M.empty                                                              
                                                                                   
initialSEnv :: SEnv                                                                
initialSEnv = M.empty      

initS :: S                                                                         
initS = S (initialFEnv, initialVEnv, initialSEnv, initialStore)                    
                                                                                   
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
   show MajaVoid = show "void"                                              
   show (MajaArray a) = show a               
   show (MajaTuple a) = show a                                 
   show (MajaStruct v m) = show v ++ ", " ++ show m           
   show (MajaArrayEval a) = show a               
   show (MajaTupleEval a) = concat ["(", (intercalate "," $ map show a), ")"]
   show (MajaStructEval (Ident v) m)    = concat ["struct ", v, ": {", desc, "}"]
      where
         desc = intercalate "," (map (\(Ident k,v) -> show k ++ ": " ++ show v) $ M.assocs m)
                                                                                
execProg :: Program -> IO ()                                                    
execProg (Prog p) = execProg' p initS                                           
   where                                                               
      execProg' [] s = return ()
      execProg' (p:ps) (S(f,e,s,st)) = do                              
         x <- execStmt p (S(f,e,s,st))      
         execProg' ps x                     
                                                                                
majaBOp :: (Bool -> Bool -> Bool) -> ExpResult -> ExpResult -> ExpResult        
majaBOp f (MajaBool b1) (MajaBool b2) = MajaBool $ f b1 b2                      
                                                                                
majaDiv :: ExpResult -> ExpResult -> ExpResult                                  
(majaDiv) (MajaInt i1) (MajaInt i2) = MajaInt $ i1 `div` i2                     
                                                                                
majaNot :: ExpResult -> ExpResult                                               
majaNot (MajaBool b) = MajaBool $ not b                                         

--------------  
--LOCAL ENVS--
--------------

localEnv :: MonadState S m => m t -> m ()                                       
localEnv f = do                                                                 
   S (fenv, venv, senv, store) <- get                               
   exec <- f                                                        
   S (_, _, _, store') <- get                                       
   put $ S (fenv, venv, senv, store')                               
                                                                                
localEnvF :: MonadState S m => m ExpResult -> FEnv -> VEnv -> SEnv -> m ExpResult
localEnvF f fenv venv senv = do                                                 
   S (fenv', venv', senv', store) <- get                
   put $ S (fenv, venv, senv, store)                    
   exec <- f                                            
   S (_, _, _, store') <- get                           
   put $ S (fenv', venv', senv', store')                
   return exec         

---------------------------
--ACCESS/HELPER FUNCTIONS--
---------------------------
                                                                                
getLoc :: MonadState S m => Var -> m Loc                                        
getLoc v = do                                                                   
            S (_, venv, _, _) <- get                                            
            return $ fromJust $ M.lookup v venv

getVar :: MonadState S m => Var -> m ExpResult                                  
getVar v = do                                                                   
             loc <- getLoc v                                                    
             S(_, _, _, (store, _)) <- get                                      
             return $ fromJust $ M.lookup loc store

getLocFromExp :: (MonadTrans m, MonadState S (m IO)) => Exp -> m IO Loc
getLocFromExp (EVar var) = getLoc var
getLocFromExp (EArray e1 e2) = do
   loc <- getLocFromExp e1
   S (_, _, _, (store, _)) <- get
   let MajaArray l = fromJust $ M.lookup loc store
   MajaInt i <- evalExpM e2
   if length l <= fromInteger i then
      error "<RuntimeError>: Index out of range!"
   else 
      return $ l !! fromInteger i
getLocFromExp (ESelect e v) = do
   loc <- getLocFromExp e
   S (_, _, _, (store, _)) <- get
   let MajaStruct var m = fromJust $ M.lookup loc store
   return $ fromJust $ M.lookup v m

fromLocToEval :: MonadState S m => Loc -> m ExpResult
fromLocToEval loc = do
   S(_, _, _, (store, _)) <- get                                    
   let val = fromJust $ M.lookup loc store
   case val of
      MajaArray locs -> mapM fromLocToEval locs >>= return . MajaArrayEval
      MajaStruct var m -> do
         v' <- mapM fromLocToEval (M.elems m)
         return $ MajaStructEval var $ M.fromList $ zip (M.keys m) v'
      MajaTuple locs -> mapM fromLocToEval locs >>= return . MajaTupleEval
      _ -> return val

newFun :: MonadState S m => Type -> Var -> [DeclVar] -> [Stmt] -> Exp -> m ()
newFun t v p s e = do
   S (fenv, venv, senv, (store, loc)) <- get
   let f = fix (\f' -> (Func (p, s, e, t, (M.insert v f' fenv), venv, senv)))
   put $ S (M.insert v f fenv, venv, senv, (store, loc))

assignFunParams :: (MonadTrans m, MonadState S (m IO)) => [DeclVar] -> VEnv -> [Exp] -> m IO VEnv
assignFunParams d venv p = assign' d p venv
   where
      assign' [] [] venv = return venv
      assign' ((DVar t v):ds) (p:ps) venv = do
         case t of
            TRef _ -> do
               loc <- getLocFromExp p
               assign' ds ps (M.insert v loc venv)
            _ -> do
               x <- evalExpM p
               l <- alloc x
               assign' ds ps (M.insert v l venv)  

defaultValue :: (MonadTrans m, MonadState S (m IO)) => Type -> [Int] -> m IO ExpResult
defaultValue t e = case t of
   TInt -> return $ MajaInt 0
   TBool -> return $ MajaBool False
   TStruct (TagType var) -> do
      S (fenv, venv, senv, (store, loc)) <- get
      let m = fromJust $ M.lookup var senv
      def <- mapM f (M.elems m)
      return $ MajaStructEval var $ M.fromList $ zip (M.keys m) def
   TStruct tag@(Tag var sdecl) -> do
      _ <- execDeclM (DeclS tag)                                          
      defaultValue (TStruct $ TagType var) e
   TArray typ -> do
      let x = if null e then 1 else head e
      l <- replicateM x $ defaultValue typ $ tail e 
      return $ MajaArrayEval l
   TTuple typ -> do
      l <- mapM (\t -> defaultValue t e) typ
      return $ MajaTupleEval l
   where
      f (SBasic t) = defaultValue t []
      f (SArr t i) = defaultValue t [i]

newLoc :: MonadState S m => m Loc
newLoc = do
   S (fenv, venv, senv, (store, loc)) <- get
   put $ S (fenv, venv, senv, (store, (loc+1)))
   return loc

newVar :: (MonadTrans m, MonadState S (m IO)) => Type -> Var -> [Int] -> m IO ExpResult                                 
newVar t v e = do                                                                 
   x <- defaultValue t e
   l <- alloc x
   S (fenv, venv, senv, (store, loc)) <- get
   put $ S (fenv, M.insert v l venv, senv, (store, loc))
   return x

assignToNewLoc :: MonadState S m => ExpResult -> m Loc
assignToNewLoc e = do
   S (fenv, venv, senv, (store, loc)) <- get
   put $ S (fenv, venv, senv, (M.insert loc e store, (loc + 1))) 
   return loc

assignToLoc :: MonadState S m => Loc -> ExpResult -> m ()
assignToLoc l e = do
   S (fenv, venv, senv, (store, loc)) <- get
   put $ S (fenv, venv, senv, (M.insert l e store, loc)) 

alloc :: MonadState S m => ExpResult -> m Loc
alloc (MajaArrayEval l) = do
   locs <- mapM alloc l
   let arr = MajaArray locs
   assignToNewLoc arr
alloc (MajaStructEval var m) = do
   locs <- mapM alloc $ M.elems m
   let str = MajaStruct var $ M.fromList $ zip (M.keys m) locs
   assignToNewLoc str
alloc (MajaTupleEval l) = do
   locs <- mapM alloc l
   let tup = MajaTuple locs
   assignToNewLoc tup
alloc x = assignToNewLoc x 

copy :: MonadState S m => Var -> Loc -> m ()
copy v l = do
   S (fenv, venv, senv, (store, loc)) <- get
   let l' = fromJust $ M.lookup v venv
   let ex = fromJust $ M.lookup l store
   put $ S (fenv, venv, senv, (M.insert l' ex store, loc)) 
  
assign ::(MonadTrans m,  MonadState S (m IO)) => Var -> ExpResult -> m IO ()
assign v val = do
   l <- alloc val
   copy v l

ifelse :: ExpResult -> t -> t -> t                                              
ifelse e f1 f2 = case e of                                                      
   MajaBool True -> f1                                        
   MajaBool False -> f2                                       
  
majaListLengths :: Type -> ExpResult -> [Int]
majaListLengths t a = reverse $ len t a []
   where
      len :: Type -> ExpResult -> [Int] -> [Int]
      len t (MajaArrayEval x) r = case t of
         (TArray typ) -> do
            let MajaArrayEval x' = head x
            len typ (head x) ((length x):r)
         _ -> (length x):r
      len _ _ r = r
                                                                   
evalArrM :: (MonadTrans m, MonadState S (m IO)) => [ArrM] -> m IO [ExpResult]   
evalArrM a = eval a []                                                          
   where                                                            
      eval [] l = return $ reverse l                                
      eval ((MulArr e):as) l = do                                   
         x <- evalExpM e                    
         eval as (x:l)                      
                                                                                
structFieldsToMap :: (MonadTrans m, MonadState S (m IO)) => Struct_dec -> m IO (Var, StructElem)
structFieldsToMap (StrField t v) = return (v, (SBasic t))
structFieldsToMap (StrFieldArr t v (MulArr e)) = do
   MajaInt i <- evalExpM e
   if i < 0 then error "<RuntimeError>: Array's length must be greater then 0!"
   else return (v, (SArr t (fromInteger i)))

---------------
--EXPRESSIONS--
---------------               
                                                                 
evalExp e s = execStateT (evalExpM e) s                                         
                                                                                
evalExpM :: (MonadTrans m, MonadState S (m IO)) => Exp -> m IO ExpResult        
evalExpM (EEmpty) = return MajaVoid                                             
                                                                                
evalExpM (EIArr (IArr exp)) = mapM evalExpM exp >>= return . MajaArrayEval
                                                                                
evalExpM (EITup (ITup e1 e2)) = mapM evalExpM (e1:e2) >>= return . MajaTupleEval
                                                                                
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
   if y == (MajaInt 0) then error "<RuntimeError>: Division by 0!"      
   else return $ x `majaDiv` y                          
                                                                                
evalExpM (EPreop Negative e) = do                                               
   x <- evalExpM e                                
   return $ MajaInt (-1) * x                      
                                                                                
evalExpM (EPreop LogNeg e) = do                                                 
  x <- evalExpM e                                 
  return $ majaNot x                              

evalExpM (EFunkpar (FCall f e)) = do                          
   S (fenv', _, _, _) <- get                   
   let fun@(Func (d, s, ex, _, fenv, venv, senv)) = fromJust $ M.lookup f fenv'
   venv' <- assignFunParams d venv e                       
   x <- localEnvF (execStmts s >> evalExpM ex) fenv venv' senv
   return x                                             
   where                                                
      execStmts [] = return ()                          
      execStmts (s:ss) = execStmtM s >> execStmts ss    

evalExpM (EArray v e) = do                                                      
   MajaInt i <- evalExpM e                                      
   MajaArrayEval l <- evalExpM v                                    
   if length l <= fromInteger i then
      error $ "<RuntimeError>: Index out of range!"
   else return $ l !! fromInteger i                                      

evalExpM (ESelect e v) = do                                                     
   MajaStructEval var m <- evalExpM e                                      
   return $ fromJust $ M.lookup v m
                                                                                
evalExpM (EVar v) = do
   S(fenv, venv, senv, (store, loc)) <- get
   let loc = fromJust $ M.lookup v venv
   fromLocToEval loc                                                    
                                                                                
evalExpM (EConst CTrue) = return $ MajaBool True                                
evalExpM (EConst CTrueB) = return $ MajaBool True                               
evalExpM (EConst CFalse) = return $ MajaBool False                              
evalExpM (EConst CFalseB) = return $ MajaBool False                             
evalExpM (EConst (CInt i)) = return $ MajaInt i    
               
--------------
--STATEMENTS--
--------------                                                                 

execStmt s m = execStateT (execStmtM s) m                                       
                                                                                
execStmtB :: (MonadTrans m, MonadState S (m IO)) => Block -> m IO ()            
execStmtB (SBl b) = localEnv $ execStmtM' b                                     
   where                                                   
      execStmtM' [] = return ()                            
      execStmtM' (s:ss) = execStmtM s >> execStmtM' ss     
                                                                                
execStmtM :: (MonadTrans m, MonadState S (m IO)) => Stmt -> m IO ()             
execStmtM (SAssign v e) = do 
   x <- evalExpM e
   assign v x     
                                                 
execStmtM (SBlock b) = execStmtB b                                              

execStmtM (SDeclV (DVar t v) e) = do
   x <- evalExpM e
   newVar t v []
   assign v x 

execStmtM (SDeclF d) = execDeclM d                                              

execStmtM (SAssignT t) = do                                                     
   case t of                                                                    
      TAssignN v1 v2 e -> do
         MajaTupleEval x <- evalExpM e
         mapM_ (\(DVar typ v) -> newVar typ v []) (v1:v2)
         mapM_ (\(t, v) -> assign v t) $ zip x $ map (\(DVar typ v) -> v) (v1:v2)
         return ()
      TAssign e1 e2 e3 -> do
         MajaTupleEval x <- evalExpM e3
         mapM (\(t,  v) -> assign v t) $ zip x (e1:e2)
         return ()

execStmtM (SAssignS v vf e) = do
   x <- evalExpM e
   MajaStruct var m <- getVar v
   S (_, _, senv, _) <- get
   let typM = fromJust $ M.lookup var senv
   let locOld = fromJust $ M.lookup vf m
   l <- alloc x
   S (fenv, venv, senv, (store, loc)) <- get
   let val = fromJust $ M.lookup l store
   put $ S (fenv, venv, senv, (M.insert locOld val store, loc))
                                                   
execStmtM (SAssignA v e1 e2) = do                                               
   x <- evalExpM e2
   (MajaInt i) <- evalExpM e1
   MajaArray list <- getVar v
   if length list <= fromInteger i then
      error $ "<RuntimeError>: Index out of range!"
   else do
      l <- alloc x
      S (fenv, venv, senv, (store, loc)) <- get
      let val = fromJust $ M.lookup l store
      put $ S (fenv, venv, senv, (M.insert (list !! fromInteger i) val store, loc))
                                                                    
execStmtM (SIf e b) = do                                                        
   x <- evalExpM e                                         
   ifelse x (execStmtB b) (return ())                      
                                                                                
execStmtM (SIfElse e b1 b2) = do                                                
   x <- evalExpM e                                   
   ifelse x (execStmtB b1) (execStmtB b2)            
                                                                                
execStmtM (SFor d e1 e2 s (SBl b)) = localEnv $ execStmtM (SDeclV d e1) >> execStmtM (SWhile e2 (SBl (b ++ [s])))
                                                                                
execStmtM while@(SWhile e b) = do                                               
   x <- evalExpM e                                      
   ifelse x                                             
       (execStmtB b >> execStmtM while)              
       (return ())                                   
                                                                                
execStmtM (SPrint e) = do                                                       
   x <- evalExpM e                                         
   lift $  putStrLn $ show x                               

execStmtM (SFunC f) = do                                                        
   evalExpM (EFunkpar f)                              
   return ()                                               
              
----------------
--DECLARATIONS--
----------------   
                                                                                
execDeclM :: (MonadTrans m, MonadState S (m IO)) => Decl -> m IO ()             
execDeclM (DeclV (DVar t v)) = newVar t v [] >> return ()                                     

execDeclM (DeclF (DFun t v p s e)) = newFun t v p s e                           

execDeclM (DeclA (DArr typ@(TArray t) v arrm)) = do                                 
   e <- evalArrM arrm                       
   newVar typ v $ map f e        
   return ()
   where
      f (MajaInt i) = if i > 0 then fromInteger i
         else error "<RuntimeError>: Array length must be greater then 0!"

execDeclM (DeclA (DArrI typ@(TArray t) v e)) = do                 
   x <- evalExpM $ EIArr e
   def <- newVar typ v $ majaListLengths t x 
   assign v x

execDeclM (DeclS (Tag var sdecl)) = do                                          
   S (fenv, venv, senv, (store, loc)) <- get
   m <- mapM structFieldsToMap sdecl
   put $ S (fenv, venv, (M.insert var (M.fromList m) senv), (store, loc))
