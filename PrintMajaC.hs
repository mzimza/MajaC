{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintMajaC where

-- pretty-printer generated by the BNF converter

import AbsMajaC
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Prog stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print Decl where
  prt i e = case e of
    DeclV declvar -> prPrec i 0 (concatD [prt 0 declvar, doc (showString ";")])
    DeclF declfun -> prPrec i 0 (concatD [prt 0 declfun])
    DeclA declarr -> prPrec i 0 (concatD [prt 0 declarr, doc (showString ";")])

instance Print DeclVar where
  prt i e = case e of
    DVar type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print DeclFun where
  prt i e = case e of
    DFun type_ id declvars stmts exp -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 declvars, doc (showString ")"), doc (showString "{"), prt 0 stmts, doc (showString "return"), prt 0 exp, doc (showString ";"), doc (showString "}")])

instance Print DeclArr where
  prt i e = case e of
    DArr type_ id exp -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "["), prt 0 exp, doc (showString "]")])

instance Print Stmt where
  prt i e = case e of
    SAssign id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp, doc (showString ";")])
    SAssignS id1 id2 exp -> prPrec i 0 (concatD [prt 0 id1, doc (showString "."), prt 0 id2, doc (showString "="), prt 0 exp, doc (showString ";")])
    SAssignA id exp1 exp2 -> prPrec i 0 (concatD [prt 0 id, doc (showString "["), prt 0 exp1, doc (showString "]"), doc (showString "="), prt 0 exp2, doc (showString ";")])
    SBlock block -> prPrec i 0 (concatD [prt 0 block])
    SDeclF decl -> prPrec i 0 (concatD [prt 0 decl])
    SDeclA declarr exp -> prPrec i 0 (concatD [prt 0 declarr, doc (showString "="), prt 0 exp, doc (showString ";")])
    SDeclV declvar exp -> prPrec i 0 (concatD [prt 0 declvar, doc (showString "="), prt 0 exp, doc (showString ";")])
    SIf exp block -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp, prt 0 block])
    SIfElse exp block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp, prt 0 block1, doc (showString "else"), prt 0 block2])
    SFor declvar exp1 exp2 stmt block -> prPrec i 0 (concatD [doc (showString "for"), prt 0 declvar, doc (showString "="), prt 0 exp1, doc (showString ";"), prt 0 exp2, doc (showString ";"), prt 0 stmt, prt 0 block])
    SWhile exp block -> prPrec i 0 (concatD [doc (showString "while"), prt 0 exp, prt 0 block])
    SPrint exp -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    SFunC funccall -> prPrec i 0 (concatD [prt 0 funccall, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Block where
  prt i e = case e of
    SBl stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print Exp where
  prt i e = case e of
    EEmpty -> prPrec i 0 (concatD [doc (showString " ")])
    EIArr arrayinit -> prPrec i 0 (concatD [prt 0 arrayinit])
    EITup tupleinit -> prPrec i 0 (concatD [prt 0 tupleinit])
    EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    EAnd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    EEq exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "=="), prt 4 exp2])
    ENeq exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "!="), prt 4 exp2])
    ELt exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "<"), prt 5 exp2])
    EGt exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString ">"), prt 5 exp2])
    ELe exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "<="), prt 5 exp2])
    EGe exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString ">="), prt 5 exp2])
    EAdd exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "+"), prt 6 exp2])
    ESub exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "-"), prt 6 exp2])
    EMul exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "*"), prt 7 exp2])
    EDiv exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "/"), prt 7 exp2])
    EPreop unaryoperator exp -> prPrec i 7 (concatD [prt 0 unaryoperator, prt 8 exp])
    EFunkpar funccall -> prPrec i 8 (concatD [prt 0 funccall])
    EArray id exp -> prPrec i 8 (concatD [prt 0 id, doc (showString "["), prt 0 exp, doc (showString "]")])
    ESelect exp id -> prPrec i 8 (concatD [prt 8 exp, doc (showString "."), prt 0 id])
    EVar id -> prPrec i 9 (concatD [prt 0 id])
    EConst constant -> prPrec i 9 (concatD [prt 0 constant])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print FuncCall where
  prt i e = case e of
    FCall id exps -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])

instance Print ArrayInit where
  prt i e = case e of
    IArr exps -> prPrec i 0 (concatD [doc (showString "{"), prt 0 exps, doc (showString "}")])

instance Print TupleInit where
  prt i e = case e of
    ITup exp exps -> prPrec i 0 (concatD [doc (showString "("), prt 0 exp, doc (showString ","), prt 0 exps, doc (showString ")")])

instance Print Constant where
  prt i e = case e of
    CInt n -> prPrec i 0 (concatD [prt 0 n])
    CTrue -> prPrec i 0 (concatD [doc (showString "true")])
    CFalse -> prPrec i 0 (concatD [doc (showString "false")])
    CTrueB -> prPrec i 0 (concatD [doc (showString "True")])
    CFalseB -> prPrec i 0 (concatD [doc (showString "False")])

instance Print Unary_operator where
  prt i e = case e of
    Negative -> prPrec i 0 (concatD [doc (showString "-")])
    LogNeg -> prPrec i 0 (concatD [doc (showString "!")])

instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TStruct structspec -> prPrec i 0 (concatD [prt 0 structspec])
    TTuple types -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")")])
    TRef type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "&")])
    TVoid -> prPrec i 0 (concatD [doc (showString "void")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Struct_spec where
  prt i e = case e of
    Tag id structdecs -> prPrec i 0 (concatD [doc (showString "struct"), prt 0 id, doc (showString "{"), prt 0 structdecs, doc (showString "}")])
    TagType id -> prPrec i 0 (concatD [doc (showString "struct"), prt 0 id])

instance Print Struct_dec where
  prt i e = case e of
    StrField type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])

