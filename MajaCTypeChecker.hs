--Author: Maja Zalewska
--index nr: 336088
--Type checker for MajaC programming language
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   

module MajaCTypeChecker  where

import AbsMajaC        
import Control.Monad.Reader
import Control.Monad.State 
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe 


type Var = Ident
type TVEnv = M.Map Var TCT
type TFEnv = M.Map Var ([(TCT, Var)] , TCT)
type TSEnv = M.Map Var (M.Map Var TCT)

data TCT = TCStruct Var (M.Map Var TCT) | TCInt | TCBool | TCVoid 
           | TCRef TCT | TCArray TCT | TCTuple [TCT] deriving (Eq, Ord)

instance Show TCT where
   show TCInt = "int"
   show TCBool = "bool"
   show (TCStruct (Ident v) _) = "struct " ++ show v
   show TCVoid = "void"
   show (TCRef x) = "ref " ++ show x
   show (TCArray x) = "[" ++ show x ++ "]"
   show (TCTuple l) = "tuple (" ++ show l ++ ")"

data T = T (TVEnv, TFEnv, TSEnv)

--BASIC TYPE'S ERROR MSGs
notABool x= "<TypeError>: Not a bool in logical expression. Got: " ++ show x
notAnInt x = "<TypeError>: Not an int in expression. Got: " ++ show x
notComparable x y = "<TypeError>: Values of different types: " ++ show x ++ ", " ++ show y
notABoolNorAnInt x y = "<TypeError>: not a bool nor an int - cannot compare " ++ show x ++ show y

--FUNCTION ERROR MSGs
parametersMismatched (Ident f) t1 t2= "<TypeError>: In function '" ++ f ++ "' parameters' types mismatched or not enough parameters given. Expected: " ++ show t1 ++ ", got: " ++ show t2
noFunction (Ident f) = "<TypeError>: Usage of undefined function '" ++ f ++ "'" 
returnType t1 t2 = "<TypeError>: Return types mismatched. Expected: " ++ show t2 ++ ", got: " ++ show t1 
notAReference (Ident f) t = "<TypeError>: In function '" ++ f ++ "' expected reference. Got: " ++ show t

--ARRAY ERROR MSGs
notAnArray t = "<TypeError>: Not an array: " ++ show t
wrongArrayType (Ident s) t1 t2 = "<TypeError>: In '" ++ s ++ "'' expected: " ++ show t1 ++ ", got: " ++ show t2
missingBrackets (Ident s) = "<TypeError>: Wrong array ('" ++ s ++ "') variable declaration. Probably missing '[]'"
dimensionsMismatched (Ident s) = "<TypeError> Dimensions of array: '" ++ show s ++ "' mismatched"
notAnArrayType (Ident a) t1 = "<TypeError>: " ++ a ++ ": wrong array type, expected array of ..., got: " ++ show t1   
differentTypesInInit = "<TypeError>: Arguments in init list for array of different types"
emptyInit = "<TypeError>: Empty init list for array"

--STRUCT ERROR MSGs
noSuchStruct (Ident s) = "<TypeError>: Struct '" ++ s ++ "' not defined"
noSuchField (Ident s) = "<TypeError>: There is no field: " ++ s ++ " in struct"
notAStruct = "<TypeError>: Not a struct"
selectFromDefinition = "<TypeError>: Cannot select from struct definition" --unused
structBodyNotDefined (Ident s) = "<TypeError>: Struct body for: " ++ s ++ " not defined"
redefinitionOfAField (Ident s) = "<TypeError>: Redefinition of a struct field '" ++ show s ++ "' in struct body"
structLoop (Ident s) = "<TypeError>: Loop in struct delcaration: " ++ s
structBodyDeclaration = "<TypeError>: Declaration of struct type within a struct"

--OTHER ERROR MSGs
notATuple = "<TypeError>: Not a tuple"
noSuchVariable (Ident s) = "<TypeError>: Usage of undefined variable: " ++ s
assignTypeMismatched (Ident s) t1 t2 = "<TypeError>: In assign to '" ++ s ++ "'. Expected type: " ++ show t1 ++ ", got: " ++ show t2
wrongForExp t = "<TypeError>: In for loop - expected different type of expression, got: " ++ show t
voidType = "<TypeError>: Cannot declare a void variable"
reference = "<TypeError>: Reference to reference or reference in forbidden place"

initT :: T
initT = T (M.empty, M.empty, M.empty)

checkProg :: Program -> IO ()                                                    
checkProg (Prog p) = checkProg' p initT                                           
   where                                                               
      checkProg' [] _ = return ()                                 
      checkProg' (p:ps) (T(f,e,s)) = do                              
         x <- checkStmt p (T(f,e,s))      
         checkProg' ps x

--------------------
--HELPER FUNCTIONS--
--------------------

checkBool :: MonadState T m => Exp -> Exp -> m TCT
checkBool e1 e2 = do
   x <- checkTExp e1
   if x /= TCBool then fail $ notABool x
   else do
      y <- checkTExp e2
      if y /= TCBool then fail $ notABool x
      else return TCBool

checkBoolOrInt :: MonadState T m => Exp -> Exp -> m TCT
checkBoolOrInt e1 e2 = do
   x <- checkTExp e1
   y <- checkTExp e2
   if x /= y then fail $ notComparable x y
   else
      if x /= TCInt && x /= TCBool then fail $ notABoolNorAnInt x y
      else return TCBool

checkInt :: MonadState T m => Exp -> Exp -> m TCT
checkInt e1 e2 = do
   x <- checkTExp e1
   if x /= TCInt then fail $ notAnInt x
   else do
      y <- checkTExp e2
      if y /= TCInt then fail $ notAnInt y
      else return TCInt

checkElemIsRef :: MonadState T m => TCT -> m TCT
checkElemIsRef (TCRef t) = fail $ reference
checkElemIsRef t = return t

checkAssign :: MonadState T m => Var -> m TCT -> m TCT -> m ()
checkAssign v tv te = do
   t1 <- tv
   t2 <- te
   if t1 /= t2 then fail $ assignTypeMismatched v t1 t2
   else return ()

checkStructField :: MonadState T m => Var -> (M.Map Var TCT) -> (Var, TCT) -> m (M.Map Var TCT)
checkStructField var m (v, t) = do
   case M.lookup v m of
      Just x -> fail $ redefinitionOfAField v
      Nothing -> case t of
         TCStruct var' m' -> do
            T (_, _, senv) <- get
            case M.lookup var' senv of
               Nothing -> fail $ noSuchStruct var'
               Just x -> if var' /= var then return $ M.insert v t m
                  else fail $ structLoop var' 
         _ -> return $ M.insert v t m

checkRefToRef :: Type -> Type
checkRefToRef (TRef _) = error reference
checkRefToRef x = x

toTCT :: MonadState T m => Type -> m TCT
toTCT TInt = return TCInt
toTCT TBool = return TCBool
toTCT TVoid = fail $ voidType
toTCT (TArray x) = (toTCT . checkRefToRef) x >>= return . TCArray
toTCT (TTuple x) = mapM (toTCT . checkRefToRef) x >>= return . TCTuple
toTCT (TRef x) = (toTCT . checkRefToRef) x >>= return . TCRef
toTCT (TStruct (TagType var)) = do
   T (venv, fenv, senv) <- get
   case M.lookup var senv of
      Just m -> return $ TCStruct var m
      Nothing -> fail $ noSuchStruct var
toTCT (TStruct (Tag var sdecl)) = do
   fields <- mapM getStructFields sdecl
   m <- foldM (checkStructField var) M.empty fields
   return $ TCStruct var m
   where
      getStructFields (StrField t v) = case t of
         TStruct (Tag _ _) -> fail $ structBodyDeclaration
         _ -> do  
            t' <- (toTCT . checkRefToRef) t
            return (v, t')
      getStructFields (StrFieldArr t v e) = case t of
         TStruct (Tag _ _) -> fail $ structBodyDeclaration
         _ -> do
            checkLen e 
            t' <- (toTCT . checkRefToRef) t
            return (v, t')
      checkLen (MulArr e) = do
         t <- checkTExp e
         if t == TCInt then return TCInt
         else fail $ notAnInt t 

unreference :: TCT -> TCT
unreference e = case e of
   TCRef x -> x   
   x -> x
   
localEnv :: MonadState T m => m t -> m ()
localEnv f = do
   T (venv, fenv, senv) <- get
   exec <- f
   put $ T (venv, fenv, senv)

getTypesTuple :: MonadState T m => Tuple -> m ([TCT], Exp)
getTypesTuple (TAssignN d1 d2 e) = do
   typs <- mapM (\x@(DVar t v) -> checkTDecl (DeclV x) >> toTCT t) (d1:d2)
   return (typs, e)  
getTypesTuple (TAssign v1 v2 e) = do
   typs <- mapM (checkTExp . EVar) (v1:v2)
   return (typs, e)

---------------
--EXPRESSIONS--
---------------

checkTExp :: (MonadState T m) => Exp -> m TCT 
checkTExp EEmpty = return TCVoid

checkTExp (EIArr (IArr e)) = do
   l <- mapM checkTExp e
   if null l then fail $ emptyInit
   else do
      let a = and $ map ((==) (head l)) l
      if a == True then return $ TCArray (head l)
      else fail $ differentTypesInInit

checkTExp (EITup (ITup e1 e2)) = do
   l <- mapM checkTExp (e1:e2)
   return $ TCTuple l

checkTExp (EOr e1 e2) = checkBool e1 e2
checkTExp (EAnd e1 e2) = checkBool e1 e2 
checkTExp (EEq e1 e2) = checkBoolOrInt e1 e2
checkTExp (ENeq e1 e2) = checkBoolOrInt e1 e2 
checkTExp (ELt e1 e2) = checkBoolOrInt e1 e2
checkTExp (EGt e1 e2) = checkBoolOrInt e1 e2
checkTExp (ELe e1 e2) = checkBoolOrInt e1 e2
checkTExp (EGe e1 e2) = checkBoolOrInt e1 e2
checkTExp (EAdd e1 e2) = checkInt e1 e2
checkTExp (ESub e1 e2) = checkInt e1 e2
checkTExp (EMul e1 e2) = checkInt e1 e2
checkTExp (EDiv e1 e2) = checkInt e1 e2

checkTExp (EPreop Negative e) = do
   x <- checkTExp e
   if x /= TCInt then fail $ notAnInt x
   else return TCInt

checkTExp (EPreop LogNeg e) = do
   x <- checkTExp e
   if x /= TCBool then fail $ notABool x
   else return TCBool

checkTExp (EFunkpar (FCall v args)) = do
   T (venv, fenv, senv) <- get
   let (params, retT) = fromMaybe (error $ noFunction v) $  M.lookup v fenv
   let len = length params
   if length args /= len then fail $ parametersMismatched v args params
   else do
      let types = zip args $ map fst params
      mapM checkRef' types
      return retT
   where
      sameType x p = if x == p then return p else fail $ parametersMismatched v p x
      checkRef' (a, p) = do
         x <- checkTExp a
         case p of
            TCRef t -> case a of
               EVar _ -> sameType x t
               EArray _ _ -> sameType x t
               ESelect _ _ -> sameType x t
               _ -> fail $ notAReference v a
            _ -> sameType x p               

checkTExp (EArray e1 e2) = do
   v <- checkTExp e1
   case v of
      TCArray t -> do
         i <- checkTExp e2
         if i /= TCInt then fail $ notAnInt i
         else return t
      _ -> fail $ notAnArray v 

checkTExp (ESelect e1 v) = do
   x <- checkTExp e1
   case x of
      TCStruct var m -> do 
         T (venv, fenv, senv) <- get
         let s = fromMaybe (error (noSuchStruct var)) $  M.lookup var senv 
         let r = fromMaybe (error (noSuchField v)) $ M.lookup v s 
         return r
      _ -> fail notAStruct

checkTExp (EVar v) = do
   T (venv, fenv, senv) <- get
   case M.lookup v venv of
      Just x -> return x
      Nothing -> fail $ noSuchVariable v 

checkTExp (EConst c) = do
   case c of
      CInt i -> return TCInt
      _ -> return TCBool

----------------
--DECLARATIONS--
----------------

checkTDecl :: MonadState T m => Decl -> m ()
checkTDecl (DeclV (DVar t v)) = case t of
   TRef _ -> fail $ reference
   TArray _ -> fail $ missingBrackets v
   TStruct (TagType var) -> do
      T (venv, fenv, senv) <- get
      case M.lookup var senv of
         Just x -> put $ T (M.insert v (TCStruct var x) venv, fenv, senv)
         Nothing -> fail $ noSuchStruct var
   TStruct (Tag var sdecl) -> do
      T (venv, fenv, senv) <- get
      put $ T (venv, fenv, M.delete var senv)
      t' <- toTCT t
      let TCStruct va m = t'
      T (venv, fenv, senv) <- get
      put $ T (M.insert v t' venv, fenv, M.insert var m senv)
   _ ->  do
      t' <- toTCT t
      T (venv, fenv, senv) <- get
      put $ T (M.insert v t' venv, fenv, senv)

checkTDecl (DeclF (DFun t v decls s e)) = do 
   ret <- if t == TVoid then return TCVoid else toTCT t
   checkElemIsRef ret
   params <- mapM f decls
   T (venv, fenv, senv) <- get
   let fun = (params, ret)
   let venv' = foldl (\m (t, v) -> M.insert v (unreference t) m) venv params
   put $ T (venv', M.insert v fun fenv, senv)
   mapM_ checkTStmt s
   x <- checkTExp e
   if x == ret then put $ T (venv, M.insert v fun fenv, senv)
   else fail $ returnType x ret
   where
      f (DVar t v) = do
         t' <- toTCT t
         return $ (t', v)

checkTDecl (DeclA (DArr t@(TArray x) v a)) = do
   mapM check a
   if checkSubArrays t a == False then fail $ dimensionsMismatched v
   else do
      t' <- toTCT t
      T (venv, fenv, senv) <- get
      put $ T (M.insert v t' venv, fenv, senv)
   where
      check (MulArr e) = do
         t <- checkTExp e
         if t == TCInt then
            return TCInt
         else fail $ notAnInt t
      checkSubArrays (TArray t) [] = False
      checkSubArrays (TArray t) (e:es) = checkSubArrays t es
      checkSubArrays t [] = True
      checkSubArrays t e = False

checkTDecl (DeclA (DArr t v a)) = fail $ notAnArrayType v t

checkTDecl (DeclA (DArrI typ@(TArray t) v e)) = do
   arr <- checkTExp $ EIArr e
   t' <- toTCT typ
   if arr /= t' then fail $ wrongArrayType v t' arr
   else do
      T (venv, fenv, senv) <- get
      put $ T (M.insert v t' venv, fenv, senv)

checkTDecl (DeclA (DArrI t v e)) = fail $ notAnArrayType v t

checkTDecl (DeclS tag@(Tag v decl)) = do
   T (venv, fenv, senv) <- get
   put $ T (venv, fenv, M.delete v senv)
   t <- toTCT $ TStruct tag
   let TCStruct var m = t 
   T (venv, fenv, senv) <- get
   put $ T (venv, fenv, M.insert var m senv)
       
checkTDecl (DeclS (TagType v)) = fail $ structBodyNotDefined v 

--------------
--STATEMENTS--
--------------

checkTBlock :: MonadState T m => Block -> m ()
checkTBlock (SBl s) = localEnv $ checkTStmt' s
   where
      checkTStmt' [] = return ()
      checkTStmt' (s:ss) = checkTStmt s >> checkTStmt' ss
   
checkStmt s m = execStateT (checkTStmt s) m  

checkTStmt :: MonadState T m => Stmt -> m ()
checkTStmt (SAssign v e) = checkAssign v (checkTExp $ EVar v) (checkTExp e)

checkTStmt (SAssignS v1 v2 e) = checkAssign v1 (checkTExp (ESelect (EVar v1) v2)) (checkTExp e)

checkTStmt stmt@(SAssignA v e1 e2) = do
   a <- checkTExp $ EVar v
   case a of
      TCArray typ -> do
         i <- checkTExp e1
         if i /= TCInt then fail $ notAnInt i
         else checkAssign v (return typ) (checkTExp e2)
      _ -> fail $ notAnArray v

checkTStmt (SAssignT tup) = do
      (typs, e) <- getTypesTuple tup
      t <- checkTExp e
      case t of
         TCTuple ts -> if typs /= ts then fail $ assignTypeMismatched (Ident "Tuple") typs ts
                      else return ()
         _ -> fail $ notATuple

checkTStmt (SBlock b) = checkTBlock b
checkTStmt (SDeclF d) = checkTDecl d

checkTStmt (SDeclV d@(DVar t v) e) = do
   checkTDecl $ DeclV d
   checkAssign v (toTCT t) (checkTExp e) 

checkTStmt (SIf e b) = do
   x <- checkTExp e
   if x == TCBool then checkTBlock b
   else fail $ notABool x 

checkTStmt (SIfElse e b1 b2) = do
   x <- checkTExp e
   if x == TCBool then checkTBlock b1 >> checkTBlock b2
   else fail $ notABool x

checkTStmt (SFor (DVar t' v) e1 e2 s b) = do
   t <- toTCT t'
   if t == TCInt then do
      T (venv, fenv, senv) <- get
      put $ T (M.insert v t venv, fenv, senv)
      x1 <- checkTExp e1
      if x1 == TCInt then do
         x2 <- checkTExp e2
         if x2 == TCBool then checkTStmt s >> checkTBlock b >> (put $ T (venv, fenv, senv))
         else fail $ wrongForExp e2
      else fail $ wrongForExp e1
   else fail $ wrongForExp t

checkTStmt (SWhile e b) = do
   x <- checkTExp e
   if x == TCBool then checkTBlock b
   else fail $ notABool e

checkTStmt (SPrint e) = checkTExp e >> return ()

checkTStmt (SFunC f) = do
   x <- checkTExp $ EFunkpar f
   return ()
