module SkelMajaC where

-- Haskell module generated by the BNF converter

import AbsMajaC
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog stmts -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DeclV declvar -> failure x
  DeclF declfun -> failure x
  DeclA declarr -> failure x
  DeclS structspec -> failure x
transDeclVar :: DeclVar -> Result
transDeclVar x = case x of
  DVar type_ ident -> failure x
transDeclFun :: DeclFun -> Result
transDeclFun x = case x of
  DFun type_ ident declvars stmts exp -> failure x
transArrM :: ArrM -> Result
transArrM x = case x of
  MulArr exp -> failure x
transDeclArr :: DeclArr -> Result
transDeclArr x = case x of
  DArr type_ ident arrms -> failure x
  DArrI type_ ident arrayinit -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  SAssign ident exp -> failure x
  SAssignS ident1 ident2 exp -> failure x
  SAssignA ident exp1 exp2 -> failure x
  SAssignT tuple -> failure x
  SBlock block -> failure x
  SDeclF decl -> failure x
  SDeclV declvar exp -> failure x
  SIf exp block -> failure x
  SIfElse exp block1 block2 -> failure x
  SFor declvar exp1 exp2 stmt block -> failure x
  SWhile exp block -> failure x
  SPrint exp -> failure x
  SFunC funccall -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  SBl stmts -> failure x
transTuple :: Tuple -> Result
transTuple x = case x of
  TAssignN declvar declvars exp -> failure x
  TAssign ident idents exp -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EEmpty -> failure x
  EIArr arrayinit -> failure x
  EITup tupleinit -> failure x
  EOr exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  ENeq exp1 exp2 -> failure x
  ELt exp1 exp2 -> failure x
  EGt exp1 exp2 -> failure x
  ELe exp1 exp2 -> failure x
  EGe exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EPreop unaryoperator exp -> failure x
  EFunkpar funccall -> failure x
  EArray exp1 exp2 -> failure x
  ESelect exp ident -> failure x
  EVar ident -> failure x
  EConst constant -> failure x
transFuncCall :: FuncCall -> Result
transFuncCall x = case x of
  FCall ident exps -> failure x
transArrayInit :: ArrayInit -> Result
transArrayInit x = case x of
  IArr exps -> failure x
transTupleInit :: TupleInit -> Result
transTupleInit x = case x of
  ITup exp exps -> failure x
transConstant :: Constant -> Result
transConstant x = case x of
  CInt integer -> failure x
  CTrue -> failure x
  CFalse -> failure x
  CTrueB -> failure x
  CFalseB -> failure x
transUnary_operator :: Unary_operator -> Result
transUnary_operator x = case x of
  Negative -> failure x
  LogNeg -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TBool -> failure x
  TStruct structspec -> failure x
  TTuple types -> failure x
  TRef type_ -> failure x
  TVoid -> failure x
  TArray type_ -> failure x
transStruct_spec :: Struct_spec -> Result
transStruct_spec x = case x of
  Tag ident structdecs -> failure x
  TagType ident -> failure x
transStruct_dec :: Struct_dec -> Result
transStruct_dec x = case x of
  StrField type_ ident -> failure x
  StrFieldArr type_ ident arrm -> failure x

