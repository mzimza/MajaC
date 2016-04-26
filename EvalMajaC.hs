--Author Maja Zalewska
--Index 336088
--Evaluation file
--
--TODO: type checker -> should check if variable is void or not
--                   -> if void in function then return ; nothing else
--      check if there isn't a variable like this already, when declaring
--TODO: przy samym deklarowaniu tablicy int a[3]; i tupla (int, bool)d; nie dawac lokacji
-- narazie moze tak byc, do zmiany później
--TODO: zagnieżdzanie tablic i struktur(to chyba mam)
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


data ExpResult = MajaInt Integer | MajaBool Bool | MajaVoid | MajaLoc [Loc] 
                 | MajaArray [Loc] Type | MajaTuple [Loc] | MajaStruct Var (M.Map Var (Loc, Type))
 
type Loc = Int
type Var = Ident
type Store = (M.Map Loc ExpResult, Loc)
type VEnv = M.Map Var Loc
type FEnv = M.Map Var Func
type SEnv = M.Map Var [Struct_dec]
data Func = Func ([DeclVar], [Loc], [Stmt], Exp, Type, FEnv, VEnv, SEnv) deriving (Eq, Ord, Show)

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
   show (MajaInt i) = "MajaInt: " ++ show i
   show (MajaBool b) = "MajaBool: " ++ show b 
   show MajaVoid = show "MajaVoid"
   show (MajaLoc l) = "locs" ++ show l
   show (MajaArray a t) = "MajaArray: " ++  (show a) ++ (show t)
   show (MajaTuple a) = "MajaTuple: " ++ show a
   show (MajaStruct v m) = "MajStruct: " ++ show v ++ ", " ++ show m

execProg :: Program -> IO ()
execProg (Prog p) = execProg' p initS 
            where
               execProg' [] (S(f,e,s,st)) = do
                                             mapM_ (putStrLn . show) $ M.toList e 
                                             mapM_ (putStrLn . show) $ M.toList $ fst st
               execProg' (p:ps) (S(f,e,s,st)) = do
                                             x <- execStmt p (S(f,e,s,st))
                                             execProg' ps x 
                                          
majaBOp :: (Bool -> Bool -> Bool) -> ExpResult -> ExpResult -> ExpResult
majaBOp f (MajaBool b1) (MajaBool b2) = MajaBool $ f b1 b2

majaDiv :: ExpResult -> ExpResult -> ExpResult
(majaDiv) (MajaInt i1) (MajaInt i2) = MajaInt $ i1 `div` i2

majaNot :: ExpResult -> ExpResult
majaNot (MajaBool b) = MajaBool $ not b

getLoc :: MonadState S m => Var -> m Loc
getLoc v = do
            S (_, venv, _, _) <- get
            return $ fromMaybe (error $ "<getLoc>: Undefined variable" ++ show v) $ M.lookup v venv 

getLocAt :: MonadState S m => Var -> Int -> m Loc
getLocAt v i = do
            S (_, venv, _, _) <- get
            exp <- getValueFromLoc $ fromMaybe (error $ "<getLocAtay>: Undefined variable" ++ show v) $ M.lookup v venv 
            l <- getList exp
            return $ l !! i

getLocTypeStruct :: Monad m => ExpResult -> Var -> m (Loc, Type)
getLocTypeStruct (MajaStruct t m) v = return $ fromMaybe (error $ "<getLocStruct>: Undefined field" ++ show v ++ "in" ++ show t) $ M.lookup v m 

getVar :: MonadState S m => Var -> m ExpResult
getVar v = do
             loc <- getLoc v
             S(_, _, _, (store, _)) <- get
             return $ fromMaybe (error "<getVar>: Undefined variable") $ M.lookup loc store 

getValueFromLoc :: MonadState S m => Loc -> m ExpResult
getValueFromLoc loc = do
               S(_, _, _, (store, _)) <- get
               return $ fromMaybe (error "<getVar>: Undefined variable") $ M.lookup loc store 

assign :: MonadState S m => ExpResult -> Loc -> m ()
assign e loc = do
               S (fenv, venv, senv, (store, l)) <- get
               case M.lookup loc store of
                  Just expr -> put $ S (fenv, venv, senv, (M.insert loc e store, l))
                  Nothing -> error ("Location unused")

newTup :: MonadState S m => [Type] -> Var -> m ()
newTup t v = do
                S(fenv, venv, senv, (store, loc)) <- get
                (tup, (store', l)) <- assign' [] t (store, (loc+1))
                put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc tup store', l))
                  where
                     assign' l [] (store, loc) = return (MajaTuple $ reverse l , (store, loc))
                     assign' l (t:ts) (store, loc) =
                        do
                           case t of      
                              TInt -> assign' (loc:l) ts ((M.insert loc (MajaInt 0) store), loc+1)
                              TBool -> assign' (loc:l) ts ((M.insert loc (MajaBool False) store), loc+1)
                              _ -> fail ("<newTup>: Tuples can only have int or bool inside");

newVar :: MonadState S m => Type -> Var -> m ()
newVar t v = do
            S(fenv, venv, senv, (store, loc)) <- get
            case t of
              TInt -> put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc (MajaInt 0) store, (loc + 1)))
              TBool -> put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc (MajaBool False) store, (loc + 1)))
              TTuple l -> newTup l v 
              TStruct (Tag var sdecl) -> put $ S (fenv, (M.insert v loc venv), (M.insert var sdecl senv), (M.insert loc (MajaStruct var M.empty) store, (loc + 1)))
              TStruct (TagType var) -> do  
                     let _ = fromMaybe (error $ "<newVar>: Undefined struct type" ++ show var) $ M.lookup var senv
                     put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc (MajaStruct var M.empty) store, (loc + 1)))
               
--TODO tu jednak rozdzielic na typy, wtedy nie bedzie problemu z lokacjami                      
newVarAssign :: MonadState S m => Type -> Var -> ExpResult -> m ()
newVarAssign t v e = do
            S(fenv, venv, senv, (store, loc)) <- get
            put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc e store, (loc + 1)))
            {--case t of
              TInt -> put $ S (fenv, (M.insert v loc venv), (M.insert loc e store, (loc + 1)))
              TBool -> put $ S (fenv, (M.insert v loc venv), (M.insert loc e store, (loc + 1)))
              TTuple -> put $ S (fenv, (M.insert v loc venv), (M.insert loc e --}

newStructAssign :: MonadState S m => Struct_spec -> Var -> ExpResult -> m ()
newStructAssign (Tag stype sdecl) var e = do
               S(fenv, venv, senv, (store, loc)) <- get
               put $ S (fenv, venv, (M.insert stype sdecl senv), (store, loc))
               str <- locsToStruct stype e
               S(fenv, venv, senv, (store, loc)) <- get
               put $ S (fenv, (M.insert var loc venv), senv, (M.insert loc str store, (loc + 1)))
newStructAssign (TagType stype) var e = do
               S(fenv, venv, senv, (store, loc)) <- get
               put $ S (fenv, (M.insert var loc venv), senv, (M.insert loc e store, (loc + 1)))

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

assignLocParams :: MonadState S m => [DeclVar] -> m [Loc]
assignLocParams d = assign' d []
                    where
                        assign' [] l = return $ reverse l
                        assign' ((DVar t v):ds) l = do
                              S(fenv, venv, senv, (store, loc)) <- get
                              case t of
                                 TInt -> do
                                          put $ S (fenv, venv, senv, (M.insert loc (MajaInt 0) store, (loc + 1)))  
                                          assign' ds $ loc : l
                                 TBool -> do
                                           put $ S (fenv, venv, senv, (M.insert loc (MajaBool False) store, (loc + 1)))
                                           assign' ds $ loc : l
                                 
                                 _ -> assign' ds l
                              
newFun :: MonadState S m => Type -> Var -> [DeclVar] -> [Stmt] -> Exp -> m()
newFun t v d s e = do
                  l <- assignLocParams d
                  S(fenv, venv, senv, (store, loc)) <- get
                  let f = fix (\f' -> (Func (d, l, s, e, t, (M.insert v f' fenv), venv, senv)))
                  put $ S (M.insert v f fenv, venv, senv, (store, loc))  

getFun :: MonadState S m => Var -> m Func
getFun f = do
            S (fenv, _, _, _) <- get
            return $ fromMaybe (error "<getFun>: Undefined variable") $ M.lookup f fenv 

--TODO structs as parameters
assignFunParams :: (MonadTrans m, MonadState S (m IO)) => Func -> [Exp] -> m IO VEnv 
assignFunParams (Func (d, l, _, _, _, _, venv, senv)) p = assign' d l p venv
   where
      assign' [] _ _ venv = return venv
      assign' ((DVar t v):ds) l (p:ps) venv = do
         case t of
            TInt -> do
                     x <- evalExpM p
                     let (l':ls) = l
                     assign x l'
                     assign' ds ls ps (M.insert v l' venv)
            TBool -> do 
                        x <- evalExpM p
                        let (l':ls) = l
                        assign x l'
                        assign' ds ls ps (M.insert v l' venv)
            TRef _ -> do
                        case p of
                           EVar var -> do
                              loc <- getLoc var
                              assign' ds l ps (M.insert v loc venv)
                           EArray (EVar var) e -> do
                              (MajaInt i) <- evalExpM e
                              loc <- getLocAt var $ fromInteger i
                              assign' ds l ps (M.insert v loc venv)
                           _ -> error ("<assignFunParams> Wrong usage of reference") 

{--
newArr2 :: (MonadTrans m, MonadState S (m IO)) => Type -> Var -> Exp -> m IO () 
newArr2 t v e = do
                  S(fenv, venv, senv, (store, loc)) <- get
                  x <- evalExpM e
                  (arr, (store', l)) <- assign' [] x (store, (loc+1)) t
                  put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc arr store', l))
                  where
                     assign' l 0 (store, loc) t = return ((MajaArray (reverse l) t) , (store, loc))
                     assign' l x (store, loc) t =
                        do
                           case t of      
                              TInt -> assign' (loc:l) (x-1) ((M.insert loc (MajaInt 0) store), loc+1) t
                              TBool -> assign' (loc:l) (x-1) ((M.insert loc (MajaBool False) store), loc+1) t
--}

-- t-> type x -> dlugosc obecnie tworzonej listy s-> store l -> loc e -> pozostala lista dlugosci
initArr :: MonadState S m => Type -> ExpResult -> Store -> [ExpResult] -> m (ExpResult, Store)
initArr t (MajaInt x) (s, l) e = assign' t [] x (s, l) e
                  where
                     assign' t l 0 (store, loc) _ = return ((MajaArray (reverse l) t), (store, loc))
                     assign' t l x (store, loc) e = do
                        case t of
                           TInt -> assign' t (loc:l) (x-1) ((M.insert loc (MajaInt 0) store), (loc+1)) e
                           TBool -> assign' t (loc:l) (x-1) ((M.insert loc (MajaBool False) store), (loc+1)) e
                           TStruct (TagType var) -> do
                              S(_, _ , senv, _) <- get
                              let _ = fromMaybe (error $ "<newVar>: Undefined struct type" ++ show var) $ M.lookup var senv
                              assign' t (loc:l) (x-1) ((M.insert loc (MajaStruct var M.empty) store), (loc+1)) e
                           TArray typ -> do
                              case e of
                                 (a:as) -> do
                                    (arr, (store', l')) <- initArr typ a (store, (loc+1)) as
                                    assign' t (loc:l) (x-1) ((M.insert loc arr store'), (l'+1)) e
                                 [] -> do
                                    fail $ "<initArr-TArray>: Length of subarray not specified" ++ (show l) ++ (show x)   

newArr :: MonadState S m => Type -> Var -> [ExpResult] -> m ()                            
newArr t v e = do   
                  S(fenv, venv, senv, (store, loc)) <- get
                  (arr, (store', l)) <- initArr t (head e) (store, (loc+1)) (tail e)
                  put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc arr store', l))


{--
newArrAssign :: MonadState S m => Type -> Var -> ExpResult -> m()
newArrAssign t v arr@(MajaArray l typ) = 
                     if t == typ then do
                        S(fenv, venv, senv, (store, loc)) <- get
                        put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc arr store, (loc + 1)))
                     else --do type checkera
                        error ("<newArrAssign>: Types of array and init list's don't match")
--}


newSubArrAssign :: MonadState S m => Type -> ExpResult -> m Loc
newSubArrAssign t locs@(MajaLoc l) = do
                        S(fenv, venv, senv, (store, loc)) <- get
                        put $ S (fenv, venv, senv, (M.insert loc (MajaArray l t) store, (loc + 1)))
                        return loc
{--                        case t of
                           TArray typ -> do
                              arr <- newSubArrAssign typ locs
                              S(fenv, venv, senv, (store, loc)) <- get
                              put $ S (fenv, venv, senv, (M.insert loc (MajaArray arr typ) store, (loc + 1)))
                           _ -> do
                              arr@(MajaArray 
                              S(fenv, venv, senv, (store, loc)) <- get
                              put $ S (fenv, venv, senv, (M.insert loc (MajaArray arr typ) store, (loc + 1)))
--}
newArrAssign :: MonadState S m => Type -> Var -> ExpResult -> m()
newArrAssign t v locs@(MajaLoc l) = do 
                        S(fenv, venv, senv, (store, loc)) <- get
                        put $ S (fenv, (M.insert v loc venv), senv, (M.insert loc (MajaArray l t) store, (loc + 1)))

assignArr :: MonadState S m => Var -> ExpResult -> ExpResult -> m ()
assignArr v (MajaInt i) e = do
                     var <- getVar v
                     l <- getList var
                     if length l <= fromInteger i then error ("<assignArr>: Index out of range")
                     else assign e (l !! fromInteger i)

getList :: MonadState S m => ExpResult -> m [Loc]
getList e = case e of
               (MajaArray l _ ) -> return l
               (MajaTuple l) -> return l
               (MajaLoc l) -> return l

getElemAt :: MonadState S m => ExpResult -> ExpResult -> m ExpResult
getElemAt var (MajaInt i) = do
             --              var <- getVar v
                           l <- getList var
                           if length l <= fromInteger i then error ("<getElemAt>: Index out of range")
                           else getValueFromLoc (l !! fromInteger i)
{--
assignArrAll e = do
                  assign' e [] TVoid
                  where
                     assign' [] l t = return $ MajaArray (reverse l) TInt
                     assign' (e:es) l t = do
                         S(fenv, venv, senv, (store, loc)) <- get
                         x <- evalExpM e
                         put $ S (fenv, venv, senv, (M.insert loc x store, (loc + 1)))
                         case x of
                           MajaInt i -> assign' es (loc:l) TInt
                           MajaBool b -> assign' es (loc:l) TBool
                           _ -> error ("<assignArrAll>: Wrong expression type");
--}

assignTupAll :: (MonadTrans m, MonadState S (m IO)) => Exp -> [Exp] -> m IO ExpResult
assignTupAll e1 e2 = do
                  assign' (e1:e2) []
                  where
                     assign' [] l = return $ MajaTuple (reverse l)
                     assign' (e:es) l = do
                         x <- evalExpM e
                         S(fenv, venv, senv, (store, loc)) <- get
                         put $ S (fenv, venv, senv, (M.insert loc x store, (loc + 1)))
                         assign' es (loc:l)


assignLocs :: (MonadTrans m, MonadState S (m IO)) => [Exp] -> m IO ExpResult
assignLocs e = do
                  assign' e []
                  where
                     assign' [] l = return $ MajaLoc (reverse l)
                     assign' (e:es) l = do
                         x <- evalExpM e
                         S(fenv, venv, senv, (store, loc)) <- get
                         --lift $ putStrLn $ "assignLocs: " ++ show x ++ ", to loc: " ++ show loc
                         put $ S (fenv, venv, senv, (M.insert loc x store, (loc + 1)))
                         assign' es (loc:l)

locsToTuple :: Monad m => ExpResult -> m ExpResult
locsToTuple (MajaLoc l) = return $ MajaTuple l

locsToArray :: Monad m => Type -> ExpResult -> m ExpResult
locsToArray t (MajaLoc l) = return $ MajaArray l t

locsToStruct :: MonadState S m => Var -> ExpResult -> m ExpResult
locsToStruct var (MajaLoc l) = do
                              S(fenv, venv, senv, (store, loc)) <- get
                              let s = fromMaybe (error $ "<newVar>: Undefined struct type" ++ show var) $ M.lookup var senv
                              assign' s l M.empty var
                              where
                                 assign' [] _ m var = return $ MajaStruct var m
                                 assign' _ [] _ _ = error ("<locsToStruct>: Not enough values specified")
                                 assign' ((StrField t s):ss) (l:ls) m var = do
                                    assign' ss ls (M.insert s (l, t) m) var            

ifelse :: ExpResult -> t -> t -> t
ifelse e f1 f2 = case e of
                     MajaBool True -> f1
                     MajaBool False -> f2

evalExp e s = execStateT (evalExpM e) s

evalExpM :: (MonadTrans m, MonadState S (m IO)) => Exp -> m IO ExpResult
evalExpM (EEmpty) = return MajaVoid

evalExpM (EIArr (IArr exp)) = assignLocs exp

evalExpM (EITup (ITup e1 e2)) = assignLocs (e1:e2) >>= locsToTuple

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

evalExpM (EFunkpar (FCall f e)) = do
                           fun@(Func (d, l, s, ex, _, fenv, venv, senv)) <- getFun f
                           venv <- assignFunParams fun e
                           x <- localEnvF (execStmts s >> evalExpM ex) fenv venv senv
                           return x
                           where
                              execStmts [] = return ()
                              execStmts (s:ss) = execStmtM s >> execStmts ss
                           
evalExpM (EArray v e) = do
                           i <- evalExpM e
                           var <- evalExpM v
                           --case v of
                           --   (EVar var) -> getElemAt var i
                            --  _ -> do
                              --      var <- evalExpM v
                           getElemAt var i 
                                                      
evalExpM (ESelect e v) = do
                           x <- evalExpM e
                           case x of
                              (MajaStruct var m) -> do
                                                      (loc, typ) <- getLocTypeStruct x v
                                                      val <- getValueFromLoc loc
                                                      return val
                              _ -> fail $ "<ESelect>: Wrong usage of select. Works only on structs" ++ show x


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
--TODO dodaj rózne przypisywania w zależności od typu zmiennej
--     zeby lokacje sie nie powielały
execStmtM (SAssign v e) = do
                           loc <- getLoc v
                           val <- getValueFromLoc loc
                           case val of
                              (MajaArray _ t) -> evalExpM e >>= locsToArray t >>= (\x -> assign x loc)
                              (MajaStruct t m) -> evalExpM e >>= locsToStruct t >>= (\x -> assign x loc)
                              _ -> evalExpM e >>= (\x -> assign x loc)  

--1)wez structa z sven 2)zobacz jakiego typu ma byc to do przypisania
--3) wyewaaluj odpowiednio e i przypisz
execStmtM (SAssignS v vf e) = do
                                 loc <- getLoc v
                                 struct <- getValueFromLoc loc
                                 (l, typ) <- getLocTypeStruct struct vf
                                 case typ of
                                    (TStruct (TagType t)) -> do
                                       x <- evalExpM e
                                       case x of
                                          (MajaStruct var m) -> assign x l
                                          (MajaLoc locs) -> locsToStruct t x >>= (\x -> assign x l)
                                    (TStruct (Tag var s)) -> fail $ "<execStmtM-structAssign>: Cannot declare struct type inside a struct"
                                    TInt -> evalExpM e >>= (\x -> assign x l)
                                    TBool -> evalExpM e >>= (\x -> assign x l)  
                                    (TTuple t) -> evalExpM e >>= (\x -> assign x l)  
                                    _ -> evalExpM e >>= locsToArray TInt >>= (\x -> assign x l)
                                 

execStmtM (SAssignA v e1 e2) = do
                                 i <- evalExpM e1
                                 x <- evalExpM e2
                                 assignArr v i x 

execStmtM (SBlock b) = execStmtB b

--Decl/DeclArr/DeclVar

execStmtM (SDeclV (DVar t v) e) = case t of
                                    (TStruct tag@(Tag var s)) -> evalExpM e >>= newStructAssign tag v
                                    (TStruct tag@(TagType var)) -> do
                                       x <- evalExpM e
                                       case x of
                                          (MajaStruct var m) -> newStructAssign tag v x
                                          (MajaLoc locs) -> locsToStruct var x >>= newStructAssign tag v
                                    _ -> evalExpM e >>= newVarAssign t v

execStmtM (SDeclF d) = execDeclM d

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
                        x <- evalExpM (EFunkpar f)
                        return ()

--evalArrM :: (MonadTrans m, MonadState S (m IO)) => [ArrM] -> m [ExpResult]
evalArrM a = eval a []
               where
                  eval [] l = return $ reverse l
                  eval ((MulArr e):as) l = do
                                             x <- evalExpM e
                                             eval as (x:l)

execDeclM :: (MonadTrans m, MonadState S (m IO)) => Decl -> m IO ()
execDeclM (DeclV (DVar t v)) = newVar t v

execDeclM (DeclF (DFun t v p s e)) = newFun t v p s e 

execDeclM (DeclA (DArr (TArray t) v arrm)) = do
                                       e <- evalArrM arrm
                                       newArr t v e

--TODO typechecker czy dlugosc tablicy == dlugosc listy inicjalizujacej
execDeclM (DeclA (DArrI (TArray t) v e)) = do
                                    locs@(MajaLoc l) <- evalExpM (EIArr e)
                                    case t of
                                       TStruct (TagType var) -> do
                                          x <- getValueFromLoc $ head l
                                          assignS var l
                                          newArrAssign t v locs
                                       TArray typ -> do
                                          locs' <- assignA typ l []
                                          newArrAssign t v locs'
                                       _ -> newArrAssign t v locs
                                     where
                                       assignS v [] = return ()
                                       assignS v (l:ls) = do
                                          x <- getValueFromLoc l
                                          --TODO do typecheckera ze jak cos innego niz MajaLoc list to zla ilosc nawiasow
                                          locs@(MajaLoc list) <- getValueFromLoc l
                                          str <- locsToStruct v locs
                                          assign str l
                                          assignS v ls
                                       assignA t [] l = return $ MajaLoc (reverse l)
                                       assignA t (l:ls) l' = do
                                          locs@(MajaLoc list) <- getValueFromLoc l
                                          case t of
                                             TArray typ -> do
                                                locs' <- assignA typ list []
                                                newLoc <- newSubArrAssign t locs'
                                                assignA t ls (newLoc:l')
                                             TStruct (TagType var) -> do
                                                assignS var list
                                                newLoc <- newSubArrAssign t locs
                                                assignA t ls (newLoc:l')
                                             _ -> do
                                                newLoc <- newSubArrAssign t locs
                                                assignA t ls (newLoc:l')

--TODO TypeChecker <- sprawdź czy takiego już nie ma
execDeclM (DeclS (Tag var sdecl)) = do
                                       S (fenv, venv, senv, (store, loc)) <- get
                                       put $ S (fenv, venv, (M.insert var sdecl senv), (store, loc))
   
--TODO to powinien robic TypeChecker
execDeclM (DeclS (TagType var)) = fail $ "<execDeclM-DeclStruct>: Body of struct not specified"
