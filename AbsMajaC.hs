

module AbsMajaC where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Prog [Stmt]
  deriving (Eq, Ord, Show, Read)

data Decl = DeclV DeclVar | DeclF DeclFun | DeclA DeclArr
  deriving (Eq, Ord, Show, Read)

data DeclVar = DVar Type Ident
  deriving (Eq, Ord, Show, Read)

data DeclFun = DFun Type Ident [DeclVar] [Stmt] Exp
  deriving (Eq, Ord, Show, Read)

data DeclArr = DArr Type Ident Exp
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SAssign Ident Exp
    | SAssignS Ident Ident Exp
    | SAssignA Ident Exp Exp
    | SBlock Block
    | SDeclF Decl
    | SDeclA DeclArr Exp
    | SDeclV DeclVar Exp
    | SIf Exp Block
    | SIfElse Exp Block Block
    | SFor DeclVar Exp Exp Stmt Block
    | SWhile Exp Block
    | SPrint Exp
  deriving (Eq, Ord, Show, Read)

data Block = SBl [Stmt]
  deriving (Eq, Ord, Show, Read)

data Exp
    = EIArr ArrayInit
    | EITup TupleInit
    | EOr Exp Exp
    | EAnd Exp Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELe Exp Exp
    | EGe Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EPreop Unary_operator Exp
    | EFunkpar Exp [Exp]
    | EArray Exp Exp
    | ESelect Exp Ident
    | EVar Ident
    | EConst Constant
  deriving (Eq, Ord, Show, Read)

data ArrayInit = IArr [Exp]
  deriving (Eq, Ord, Show, Read)

data TupleInit = ITup Exp [Exp]
  deriving (Eq, Ord, Show, Read)

data Constant = CInt Integer | CTrue | CFalse | CTrueB | CFalseB
  deriving (Eq, Ord, Show, Read)

data Unary_operator = Negative | LogNeg
  deriving (Eq, Ord, Show, Read)

data Type
    = TInt | TBool | TStruct Struct_spec | TTuple [Type] | TRef Type
  deriving (Eq, Ord, Show, Read)

data Struct_spec = Tag Ident [Struct_dec] | TagType Ident
  deriving (Eq, Ord, Show, Read)

data Struct_dec = StrField Type Ident
  deriving (Eq, Ord, Show, Read)

