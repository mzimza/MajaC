{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParMajaC where
import AbsMajaC
import LexMajaC
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn37 :: (Ident) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Ident)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Integer) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Integer)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Program) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Program)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ([Ident]) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> ([Ident])
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([Stmt]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([Stmt])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([DeclVar]) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ([DeclVar])
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([ArrM]) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ([ArrM])
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Decl) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Decl)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (DeclVar) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (DeclVar)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (DeclFun) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (DeclFun)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (ArrM) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (ArrM)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (DeclArr) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (DeclArr)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Stmt) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Stmt)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Block) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Block)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Tuple) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Tuple)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Exp) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Exp)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (Exp) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (Exp)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (Exp) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (Exp)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (Exp) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (Exp)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Exp) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Exp)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (Exp) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (Exp)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (Exp) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (Exp)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (Exp) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (Exp)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Exp) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Exp)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (Exp) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (Exp)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: ([Exp]) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> ([Exp])
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: (FuncCall) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> (FuncCall)
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (ArrayInit) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (ArrayInit)
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: (TupleInit) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> (TupleInit)
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: (Constant) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> (Constant)
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: (Unary_operator) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> (Unary_operator)
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: (Type) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> (Type)
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: ([Type]) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> ([Type])
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: (Struct_spec) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> (Struct_spec)
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: ([Struct_dec]) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> ([Struct_dec])
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyIn72 :: (Struct_dec) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> (Struct_dec)
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x25\x03\x00\x00\x4b\x04\x35\x03\x4b\x04\x4b\x04\x4b\x04\x35\x03\x4b\x04\x73\x00\x2b\x03\x45\x03\x12\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x41\x00\x41\x00\x12\x00\x1b\x03\x11\x03\x2a\x03\x50\x04\x13\x00\x4b\x04\x4b\x04\x12\x03\x4b\x04\x4b\x04\x0f\x03\x00\x00\x0e\x00\x00\x00\x0e\x03\x4b\x04\x4b\x04\x00\x00\x00\x00\x0c\x03\x00\x00\x0b\x03\x23\x03\x08\x03\x89\x00\x08\x03\x04\x00\x08\x03\x00\x00\x00\x00\x00\x00\x08\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x03\x12\x00\x08\x03\x12\x00\x28\x03\x05\x03\x27\x03\x22\x03\xfe\x02\x14\x03\xff\xff\xc3\x02\xd3\x01\x53\x00\x00\x00\x82\x00\x00\x00\xee\x02\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x12\x00\x00\x00\x00\x00\xee\x02\x12\x00\x9d\x00\xee\x02\x30\x00\xfb\xff\x86\x02\x08\x00\x05\x00\x8f\x00\xee\x02\xee\x02\x83\x00\xee\x02\x00\x00\x48\x00\x00\x00\x90\x01\x00\x00\x09\x03\xec\x02\x00\x00\x00\x00\x07\x03\x0e\x00\x06\x03\x83\x00\x4b\x04\x12\x00\x0d\x03\x12\x00\xe7\x02\x0e\x00\xe7\x02\x12\x00\xe7\x02\x0e\x00\xe7\x02\x0e\x00\xe7\x02\x04\x03\xe6\x02\xf8\x02\xe4\x02\x03\x03\x25\x00\xf0\x02\xdb\x02\xdb\x02\x73\x00\x00\x00\xd1\x02\x4b\x04\x00\x00\x00\x00\x00\x00\x00\x00\xf2\x02\xdc\x02\xd8\x02\xc1\x02\x12\x00\xc1\x02\xca\x02\xd0\x02\xcb\x02\x09\x00\xd2\x02\x00\x00\x4a\x00\x00\x00\x00\x00\x12\x00\x12\x00\xb0\x02\x12\x00\x12\x00\x5f\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\xb0\x02\x12\x00\xcd\x02\x78\x01\x82\x00\x12\x00\xa9\x02\xbd\x02\x4b\x04\x4b\x04\x95\x02\x6d\x00\xa5\x02\x00\x00\x00\x00\x4b\x04\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x9e\x02\x00\x00\x00\x00\x00\x00\x53\x00\x53\x00\xd3\x01\xd3\x01\xd3\x01\xd3\x01\xc3\x02\xc3\x02\xff\xff\xa3\x02\x00\x00\x84\x02\x8f\x02\x87\x02\x8d\x02\x89\x02\x00\x00\x4b\x04\x01\x00\x00\x00\x4b\x04\x61\x02\x12\x00\x78\x02\x53\x02\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x02\x45\x02\x65\x02\x4d\x02\x4c\x02\x3a\x02\x35\x02\x00\x00\x00\x00\x12\x00\x00\x00\x2a\x02\x00\x00\x2d\x02\xf6\x01\x00\x00\x00\x00\x12\x00\x1e\x02\xf7\x01\xd9\x01\x04\x02\xfc\x01\x12\x00\x00\x00\x00\x00\xef\x01\x12\x00\x12\x00\x00\x00\x00\x00\x00\x00\xed\x01\x00\x00\x4f\x00\xe8\x01\xe0\x01\x73\x00\xba\x01\x00\x00\x00\x00\x12\x00\xc9\x01\x00\x00\xa8\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x7c\x00\x5f\x01\xbb\x01\x71\x02\x2e\x02\xaf\x02\x92\x02\x7e\x00\xad\x01\x02\x00\xc9\x00\x92\x01\x72\x01\xc4\x03\xe3\x03\x0c\x04\x19\x04\x42\x04\x84\x01\xe1\x01\x3e\x02\x5d\x02\x5c\x00\x58\x01\x1e\x00\x94\x01\x45\x01\x03\x00\x24\x01\x9d\x01\x5c\x02\x6f\x01\xd3\x02\x91\x02\x00\x00\x00\x00\x23\x01\x00\x00\x00\x00\xde\x00\xad\x00\x00\x00\x00\x00\x07\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x03\x00\x00\x39\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x86\x03\x00\x00\x00\x00\x00\x00\x67\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x00\x00\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x00\x00\x00\xa1\x00\x42\x00\x48\x03\x00\x00\x29\x03\x00\x00\xd3\x00\x00\x00\x0a\x03\x00\x00\xa7\x00\x00\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x0f\x02\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x40\x01\x52\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x95\x00\xeb\x02\x87\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\xd5\x00\x00\x00\x00\x00\xcc\x02\x1a\x01\x66\x00\xad\x02\x8e\x02\xae\x00\xf1\x03\xfe\x03\x34\x04\x27\x04\x65\x01\x46\x01\x27\x01\x08\x01\xc2\x01\xa3\x01\x1f\x02\x00\x02\x57\x00\x6f\x02\x00\x00\x00\x00\x00\x00\xfb\x00\x00\x00\x00\x00\xb9\x00\xce\x02\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\xe2\x00\x00\x00\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\x00\x50\x02\x00\x00\xbf\x00\x21\x01\x31\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x01\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\xd4\x01\x00\x00\x00\x00\x00\x00\xb5\x01\x96\x01\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\xa4\x00\x0d\x00\x00\x00\x00\x00\x77\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd8\xff\x00\x00\xd8\xff\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\xff\x00\x00\x00\x00\xdd\xff\x00\x00\x8a\xff\x00\x00\x00\x00\x00\x00\x8b\xff\x8c\xff\x00\x00\x87\xff\x00\x00\x80\xff\x00\x00\x85\xff\x00\x00\x00\x00\x00\x00\x8d\xff\x8e\xff\x93\xff\x00\x00\x8f\xff\x90\xff\x91\xff\x92\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\x00\x00\x9c\xff\x98\xff\xb5\xff\xb3\xff\xb1\xff\xae\xff\xa9\xff\xa6\xff\xa3\xff\xa1\xff\x9d\xff\x00\x00\xa0\xff\xb7\xff\xb6\xff\x9b\xff\x00\x00\x00\x00\xb8\xff\x9c\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\xc3\xff\x00\x00\xd0\xff\x00\x00\x00\x00\xc4\xff\xc5\xff\x00\x00\x00\x00\x8a\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd3\xff\x00\x00\xd5\xff\x00\x00\xda\xff\x00\x00\x00\x00\xdb\xff\xd7\xff\x00\x00\xd6\xff\xd2\xff\xd1\xff\xcd\xff\x88\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\xff\x00\x00\xce\xff\xcd\xff\xbc\xff\xcf\xff\x00\x00\x99\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\xff\x99\xff\x00\x00\x00\x00\x00\x00\x81\xff\x82\xff\x00\x00\x7e\xff\x7d\xff\x86\xff\x81\xff\x7f\xff\x84\xff\x99\xff\x95\xff\x97\xff\x9a\xff\x00\x00\x9e\xff\xa4\xff\xa5\xff\xa7\xff\xa8\xff\xaa\xff\xac\xff\xab\xff\xad\xff\xb0\xff\xaf\xff\xb2\xff\xb4\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\xd6\xff\x00\x00\x89\xff\xd6\xff\x00\x00\x00\x00\xc1\xff\x00\x00\xbe\xff\xcb\xff\xd4\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\x96\xff\x00\x00\xc8\xff\x00\x00\x9f\xff\x00\x00\x00\x00\x83\xff\x94\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\xbd\xff\x00\x00\x00\x00\x00\x00\xc9\xff\xd8\xff\xc7\xff\x00\x00\xc6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xba\xff\x00\x00\x00\x00\xbf\xff\x00\x00\xcc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x08\x00\x01\x00\x0a\x00\x05\x00\x03\x00\x1b\x00\x04\x00\x02\x00\x0a\x00\x03\x00\x0b\x00\x0d\x00\x04\x00\x11\x00\x03\x00\x09\x00\x01\x00\x01\x00\x14\x00\x15\x00\x05\x00\x17\x00\x11\x00\x0d\x00\x1a\x00\x0a\x00\x0a\x00\x00\x00\x0d\x00\x1d\x00\x1f\x00\x21\x00\x21\x00\x29\x00\x24\x00\x14\x00\x15\x00\x27\x00\x28\x00\x05\x00\x01\x00\x1a\x00\x29\x00\x29\x00\x05\x00\x27\x00\x29\x00\x0a\x00\x21\x00\x0a\x00\x27\x00\x24\x00\x07\x00\x1a\x00\x27\x00\x28\x00\x16\x00\x0c\x00\x18\x00\x14\x00\x15\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1a\x00\x20\x00\x05\x00\x22\x00\x23\x00\x24\x00\x08\x00\x21\x00\x27\x00\x05\x00\x29\x00\x05\x00\x0d\x00\x27\x00\x28\x00\x0b\x00\x05\x00\x14\x00\x15\x00\x00\x00\x10\x00\x29\x00\x07\x00\x1a\x00\x00\x00\x01\x00\x16\x00\x0c\x00\x16\x00\x1f\x00\x21\x00\x21\x00\x05\x00\x16\x00\x00\x00\x18\x00\x27\x00\x28\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x03\x00\x22\x00\x23\x00\x24\x00\x18\x00\x16\x00\x27\x00\x18\x00\x05\x00\x1d\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x02\x00\x20\x00\x04\x00\x22\x00\x23\x00\x24\x00\x17\x00\x26\x00\x27\x00\x09\x00\x05\x00\x16\x00\x00\x00\x18\x00\x03\x00\x0b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x09\x00\x20\x00\x0d\x00\x22\x00\x23\x00\x24\x00\x16\x00\x16\x00\x27\x00\x18\x00\x00\x00\x1f\x00\x00\x00\x21\x00\x1d\x00\x00\x00\x0d\x00\x20\x00\x00\x00\x22\x00\x08\x00\x00\x00\x0b\x00\x08\x00\x27\x00\x07\x00\x08\x00\x09\x00\x00\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x16\x00\x25\x00\x07\x00\x08\x00\x09\x00\x29\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x1a\x00\x21\x00\x1f\x00\x20\x00\x21\x00\x1f\x00\x05\x00\x21\x00\x29\x00\x08\x00\x1a\x00\x00\x00\x00\x00\x01\x00\x1f\x00\x1f\x00\x21\x00\x21\x00\x07\x00\x08\x00\x09\x00\x00\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x20\x00\x21\x00\x06\x00\x00\x00\x01\x00\x1f\x00\x0a\x00\x21\x00\x17\x00\x18\x00\x1a\x00\x1a\x00\x00\x00\x05\x00\x1d\x00\x1f\x00\x08\x00\x21\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x1f\x00\x20\x00\x21\x00\x1f\x00\x1f\x00\x21\x00\x21\x00\x22\x00\x23\x00\x04\x00\x00\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x1a\x00\x00\x00\x03\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x1a\x00\x1e\x00\x03\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x1a\x00\x1c\x00\x03\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x06\x00\x1a\x00\x0e\x00\x09\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x21\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x0d\x00\x1a\x00\x0d\x00\x10\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x1b\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0a\x00\x15\x00\x16\x00\x17\x00\x18\x00\x1f\x00\x1a\x00\x21\x00\x04\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x26\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x08\x00\x1a\x00\x0a\x00\x24\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x0d\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x0d\x00\x1a\x00\x0d\x00\x24\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x10\x00\x06\x00\x16\x00\x17\x00\x18\x00\x0a\x00\x1a\x00\x24\x00\x26\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x0d\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x06\x00\x06\x00\x16\x00\x17\x00\x18\x00\x0a\x00\x1a\x00\x10\x00\x06\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x06\x00\x06\x00\x16\x00\x17\x00\x18\x00\x05\x00\x1a\x00\x06\x00\x08\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x24\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x1f\x00\x0d\x00\x21\x00\x17\x00\x18\x00\x05\x00\x1a\x00\x0d\x00\x08\x00\x1d\x00\x1f\x00\x20\x00\x21\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x27\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x1f\x00\x19\x00\x21\x00\x06\x00\x0e\x00\x0f\x00\x0d\x00\x10\x00\x12\x00\x13\x00\x08\x00\x17\x00\x0d\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x29\x00\x1f\x00\x1f\x00\x21\x00\x21\x00\x23\x00\x17\x00\x07\x00\x08\x00\x09\x00\x24\x00\x0b\x00\x16\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x1f\x00\x26\x00\x21\x00\x0e\x00\x0f\x00\x06\x00\x09\x00\x12\x00\x13\x00\x27\x00\x06\x00\x09\x00\x10\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x24\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x1f\x00\x16\x00\x21\x00\x22\x00\x23\x00\x1f\x00\x17\x00\x21\x00\x22\x00\x23\x00\x05\x00\x27\x00\x09\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x29\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x09\x00\x29\x00\x16\x00\x29\x00\x29\x00\x0d\x00\x05\x00\x0d\x00\x0d\x00\x29\x00\x0d\x00\x29\x00\x04\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x25\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x09\x00\x05\x00\x05\x00\x29\x00\x05\x00\x0d\x00\x29\x00\x20\x00\x27\x00\x29\x00\x24\x00\x27\x00\x29\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x27\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x05\x00\x16\x00\x27\x00\xff\xff\xff\xff\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x05\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\x16\x00\xff\xff\x18\x00\x14\x00\x15\x00\xff\xff\xff\xff\x1d\x00\xff\xff\x1a\x00\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xa8\x00\x35\x00\xae\x00\x36\x00\xaf\x00\x55\x00\x90\x00\x05\x01\xa7\x00\xa8\x00\x36\x00\x90\x00\x76\x00\x56\x00\x0a\x01\xa9\x00\x90\x00\xba\x00\x35\x00\x35\x00\x39\x00\x3a\x00\x55\x00\xee\x00\xa9\x00\x13\x01\x3b\x00\x36\x00\x36\x00\x41\x00\x56\x00\x37\x00\x77\x00\x3c\x00\x25\x00\xff\xff\x41\x00\x39\x00\x3a\x00\x24\x00\x3d\x00\x72\x00\x35\x00\x3b\x00\xff\xff\xff\xff\x59\x00\x24\x00\xff\xff\xbe\x00\x3c\x00\x36\x00\x24\x00\x41\x00\xb0\x00\x42\x00\x24\x00\x3d\x00\x29\x00\xb1\x00\x2a\x00\x39\x00\x3a\x00\x73\x00\x74\x00\x2b\x00\x75\x00\x3b\x00\x2c\x00\x59\x00\x2d\x00\x76\x00\x66\x00\x96\x00\x3c\x00\x24\x00\xa1\x00\xff\xff\xdd\x00\x00\x01\x24\x00\x3d\x00\xa2\x00\x72\x00\x39\x00\x3a\x00\xc8\x00\xa3\x00\xff\xff\xb0\x00\x3b\x00\x56\x00\x36\x00\xa4\x00\xb1\x00\xde\x00\x7d\x00\x3c\x00\x25\x00\x72\x00\x29\x00\xd8\x00\x2a\x00\x24\x00\x3d\x00\x73\x00\x74\x00\x2b\x00\x75\x00\x12\x01\x2c\x00\x90\x00\x2d\x00\x76\x00\x66\x00\x57\x00\x29\x00\x24\x00\x2a\x00\x72\x00\x52\x00\x73\x00\x74\x00\x2b\x00\x75\x00\x87\x00\x2c\x00\x88\x00\x2d\x00\x76\x00\x66\x00\xc0\x00\xd6\x00\x24\x00\x7a\x00\x28\x00\x29\x00\x8e\x00\x2a\x00\x90\x00\xb2\x00\x73\x00\x74\x00\x2b\x00\x75\x00\xba\x00\x2c\x00\xe2\x00\x2d\x00\x76\x00\x66\x00\xb3\x00\x29\x00\x24\x00\x2a\x00\x8e\x00\x7b\x00\x97\x00\x25\x00\x2b\x00\x97\x00\xe4\x00\x2c\x00\x66\x00\x2d\x00\x98\x00\x90\x00\xb2\x00\x98\x00\x24\x00\x67\x00\x68\x00\x69\x00\x66\x00\x6a\x00\x0e\x01\x6c\x00\x6d\x00\xb3\x00\xa6\x00\x67\x00\x68\x00\x69\x00\xff\xff\x6a\x00\x89\x00\x6c\x00\x6d\x00\x7d\x00\x6e\x00\x25\x00\x99\x00\x9a\x00\x25\x00\x6f\x00\xec\x00\x70\x00\xff\xff\x83\x00\x6e\x00\x66\x00\x43\x00\x36\x00\xbc\x00\x6f\x00\x25\x00\x70\x00\x67\x00\x68\x00\x69\x00\x92\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x30\x00\xc2\x00\x25\x00\xdb\x00\x43\x00\x36\x00\x7d\x00\x81\x00\x25\x00\xb5\x00\x4d\x00\x6e\x00\x4f\x00\x9c\x00\xee\x00\x52\x00\x6f\x00\x83\x00\x70\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xf5\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x30\x00\x9a\x00\x25\x00\x7d\x00\x24\x00\x25\x00\x25\x00\xf6\x00\x2e\x00\xa4\x00\xbb\x00\x43\x00\x36\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xc5\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xcd\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x85\x00\x4f\x00\xbd\x00\xeb\x00\x52\x00\x53\x00\x43\x00\x36\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xd9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xce\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x85\x00\x4f\x00\x33\x00\xe7\x00\x52\x00\x53\x00\x43\x00\x36\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xb7\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xcf\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x85\x00\x4f\x00\x3d\x00\x86\x00\x52\x00\x53\x00\x43\x00\x36\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xd0\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xc7\x00\x4f\x00\x62\x00\xc4\x00\x52\x00\x53\x00\x43\x00\x36\x00\x12\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x2f\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x5c\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x8e\x00\x4f\x00\x64\x00\xa0\x00\x52\x00\x53\x00\x43\x00\x36\x00\x0b\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x3f\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x78\x00\xcb\x00\x4b\x00\x4c\x00\x4d\x00\x32\x00\x4f\x00\x25\x00\x84\x00\x52\x00\x53\x00\x43\x00\x36\x00\x0c\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x16\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x15\x01\xcc\x00\x4b\x00\x4c\x00\x4d\x00\xae\x00\x4f\x00\xaf\x00\x66\x00\x52\x00\x53\x00\x43\x00\x36\x00\x02\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x10\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x11\x01\x5b\x00\x4b\x00\x4c\x00\x4d\x00\x0a\x01\x4f\x00\x0e\x01\x41\x00\x52\x00\x53\x00\x43\x00\x36\x00\x08\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x04\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x05\x01\x8c\x00\xc9\x00\x4c\x00\x4d\x00\x81\x00\x4f\x00\x07\x01\xf8\x00\x52\x00\x53\x00\x43\x00\x36\x00\xfa\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x08\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xf9\x00\x80\x00\xca\x00\x4c\x00\x4d\x00\x81\x00\x4f\x00\xfa\x00\xfc\x00\x52\x00\x53\x00\x43\x00\x36\x00\xea\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xfd\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xfe\x00\xff\x00\x5a\x00\x4c\x00\x4d\x00\xe6\x00\x4f\x00\xe9\x00\x83\x00\x52\x00\x53\x00\x43\x00\x36\x00\x91\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x66\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x7d\x00\x00\x01\x25\x00\x59\x00\x4d\x00\x82\x00\x4f\x00\x02\x01\x83\x00\x52\x00\x30\x00\x31\x00\x25\x00\xc7\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x24\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x7d\x00\xea\x00\x25\x00\xf1\x00\xaa\x00\xab\x00\xf0\x00\xf2\x00\xac\x00\xad\x00\x7c\x00\xf4\x00\xf3\x00\xd6\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xa7\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xff\xff\x24\x00\x7d\x00\x25\x00\x25\x00\x26\x00\xf5\x00\x7e\x00\x7f\x00\x69\x00\xc1\x00\x6a\x00\x7a\x00\xd7\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xc4\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x6f\x00\xc5\x00\x70\x00\xaa\x00\xab\x00\xc7\x00\xe0\x00\xac\x00\xad\x00\x24\x00\xdf\x00\xe1\x00\xe2\x00\xda\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x66\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x24\x00\xde\x00\x25\x00\xc1\x00\x2e\x00\x24\x00\xe6\x00\x25\x00\x2d\x00\x2e\x00\xdd\x00\x24\x00\x8b\x00\xe3\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xff\xff\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x8c\x00\xff\xff\x7a\x00\xff\xff\xff\xff\x8e\x00\x95\x00\x9c\x00\x9e\x00\xff\xff\x9f\x00\xff\xff\xa7\x00\x91\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xa6\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\xb7\x00\xa1\x00\xa1\x00\xff\xff\x3f\x00\xbb\x00\xff\xff\x2c\x00\x24\x00\xff\xff\x41\x00\x24\x00\xff\xff\x93\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x24\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x64\x00\x7a\x00\x24\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x43\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x36\x00\x60\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\xd4\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\xd3\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\x5f\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\x5e\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\x00\x00\x00\x00\xd1\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x43\x00\x36\x00\x52\x00\x53\x00\x00\x00\xd2\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x00\x00\x28\x00\x52\x00\x53\x00\x00\x00\x00\x00\x5d\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x52\x00\x53\x00\x29\x00\x00\x00\x2a\x00\x39\x00\x3a\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x3b\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (34, 130) [
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130)
	]

happy_n_terms = 42 :: Int
happy_n_nonterms = 36 :: Int

happyReduce_34 = happySpecReduce_1  0# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn37
		 (Ident happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  1# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn38
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_36 = happySpecReduce_1  2# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsMajaC.Prog (reverse happy_var_1)
	)}

happyReduce_37 = happySpecReduce_1  3# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((:[]) happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  3# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_0  4# happyReduction_39
happyReduction_39  =  happyIn41
		 ([]
	)

happyReduce_40 = happySpecReduce_2  4# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_41 = happySpecReduce_0  5# happyReduction_41
happyReduction_41  =  happyIn42
		 ([]
	)

happyReduce_42 = happySpecReduce_1  5# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ((:[]) happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  5# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_1  6# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((:[]) happy_var_1
	)}

happyReduce_45 = happySpecReduce_2  6# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_46 = happySpecReduce_2  7# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (AbsMajaC.DeclV happy_var_1
	)}

happyReduce_47 = happySpecReduce_1  7# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (AbsMajaC.DeclF happy_var_1
	)}

happyReduce_48 = happySpecReduce_2  7# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (AbsMajaC.DeclA happy_var_1
	)}

happyReduce_49 = happySpecReduce_2  7# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (AbsMajaC.DeclS happy_var_1
	)}

happyReduce_50 = happySpecReduce_2  8# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (AbsMajaC.DVar happy_var_1 happy_var_2
	)}}

happyReduce_51 = happyReduce 11# 9# happyReduction_51
happyReduction_51 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOut41 happy_x_7 of { happy_var_7 -> 
	case happyOut52 happy_x_9 of { happy_var_9 -> 
	happyIn46
		 (AbsMajaC.DFun happy_var_1 happy_var_2 happy_var_4 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest}}}}}

happyReduce_52 = happySpecReduce_3  10# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (AbsMajaC.MulArr happy_var_2
	)}

happyReduce_53 = happySpecReduce_3  11# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsMajaC.DArr happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_54 = happyReduce 6# 11# happyReduction_54
happyReduction_54 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut64 happy_x_6 of { happy_var_6 -> 
	happyIn48
		 (AbsMajaC.DArrI happy_var_1 happy_var_2 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_55 = happyReduce 4# 12# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.SAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_56 = happyReduce 6# 12# happyReduction_56
happyReduction_56 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	case happyOut52 happy_x_5 of { happy_var_5 -> 
	happyIn49
		 (AbsMajaC.SAssignS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_57 = happyReduce 7# 12# happyReduction_57
happyReduction_57 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	case happyOut52 happy_x_6 of { happy_var_6 -> 
	happyIn49
		 (AbsMajaC.SAssignA happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_58 = happySpecReduce_1  12# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMajaC.SAssignT happy_var_1
	)}

happyReduce_59 = happySpecReduce_1  12# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMajaC.SBlock happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  12# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMajaC.SDeclF happy_var_1
	)}

happyReduce_61 = happyReduce 4# 12# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.SDeclV happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_62 = happySpecReduce_3  12# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.SIf happy_var_2 happy_var_3
	)}}

happyReduce_63 = happyReduce 5# 12# happyReduction_63
happyReduction_63 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	case happyOut50 happy_x_5 of { happy_var_5 -> 
	happyIn49
		 (AbsMajaC.SIfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_64 = happyReduce 9# 12# happyReduction_64
happyReduction_64 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut52 happy_x_4 of { happy_var_4 -> 
	case happyOut52 happy_x_6 of { happy_var_6 -> 
	case happyOut49 happy_x_8 of { happy_var_8 -> 
	case happyOut50 happy_x_9 of { happy_var_9 -> 
	happyIn49
		 (AbsMajaC.SFor happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_9
	) `HappyStk` happyRest}}}}}

happyReduce_65 = happySpecReduce_3  12# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.SWhile happy_var_2 happy_var_3
	)}}

happyReduce_66 = happyReduce 5# 12# happyReduction_66
happyReduction_66 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.SPrint happy_var_3
	) `HappyStk` happyRest}

happyReduce_67 = happySpecReduce_2  12# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsMajaC.SFunC happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  13# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 (AbsMajaC.SBl (reverse happy_var_2)
	)}

happyReduce_69 = happyReduce 8# 14# happyReduction_69
happyReduction_69 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOut52 happy_x_7 of { happy_var_7 -> 
	happyIn51
		 (AbsMajaC.TAssignN happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_70 = happyReduce 8# 14# happyReduction_70
happyReduction_70 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	case happyOut52 happy_x_7 of { happy_var_7 -> 
	happyIn51
		 (AbsMajaC.TAssign happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_71 = happySpecReduce_1  15# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn52
		 (AbsMajaC.EEmpty
	)

happyReduce_72 = happySpecReduce_1  15# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (AbsMajaC.EIArr happy_var_1
	)}

happyReduce_73 = happySpecReduce_1  15# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (AbsMajaC.EITup happy_var_1
	)}

happyReduce_74 = happySpecReduce_1  15# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  16# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMajaC.EOr happy_var_1 happy_var_3
	)}}

happyReduce_76 = happySpecReduce_1  16# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  17# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsMajaC.EAnd happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_1  17# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_3  18# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 (AbsMajaC.EEq happy_var_1 happy_var_3
	)}}

happyReduce_80 = happySpecReduce_3  18# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 (AbsMajaC.ENeq happy_var_1 happy_var_3
	)}}

happyReduce_81 = happySpecReduce_1  18# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_82 = happySpecReduce_3  19# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsMajaC.ELt happy_var_1 happy_var_3
	)}}

happyReduce_83 = happySpecReduce_3  19# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsMajaC.EGt happy_var_1 happy_var_3
	)}}

happyReduce_84 = happySpecReduce_3  19# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsMajaC.ELe happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_3  19# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (AbsMajaC.EGe happy_var_1 happy_var_3
	)}}

happyReduce_86 = happySpecReduce_1  19# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (happy_var_1
	)}

happyReduce_87 = happySpecReduce_3  20# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 (AbsMajaC.EAdd happy_var_1 happy_var_3
	)}}

happyReduce_88 = happySpecReduce_3  20# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 (AbsMajaC.ESub happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_1  20# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (happy_var_1
	)}

happyReduce_90 = happySpecReduce_3  21# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (AbsMajaC.EMul happy_var_1 happy_var_3
	)}}

happyReduce_91 = happySpecReduce_3  21# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (AbsMajaC.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_1  21# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_2  22# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsMajaC.EPreop happy_var_1 happy_var_2
	)}}

happyReduce_94 = happySpecReduce_1  22# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  23# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (AbsMajaC.EFunkpar happy_var_1
	)}

happyReduce_96 = happyReduce 4# 23# happyReduction_96
happyReduction_96 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 (AbsMajaC.EArray happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_97 = happySpecReduce_3  23# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 (AbsMajaC.ESelect happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_1  23# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_99 = happySpecReduce_1  24# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 (AbsMajaC.EVar happy_var_1
	)}

happyReduce_100 = happySpecReduce_1  24# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 (AbsMajaC.EConst happy_var_1
	)}

happyReduce_101 = happySpecReduce_3  24# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { happy_var_2 -> 
	happyIn61
		 (happy_var_2
	)}

happyReduce_102 = happySpecReduce_0  25# happyReduction_102
happyReduction_102  =  happyIn62
		 ([]
	)

happyReduce_103 = happySpecReduce_1  25# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ((:[]) happy_var_1
	)}

happyReduce_104 = happySpecReduce_3  25# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_105 = happyReduce 4# 26# happyReduction_105
happyReduction_105 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn63
		 (AbsMajaC.FCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_106 = happySpecReduce_3  27# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_2 of { happy_var_2 -> 
	happyIn64
		 (AbsMajaC.IArr happy_var_2
	)}

happyReduce_107 = happyReduce 5# 28# happyReduction_107
happyReduction_107 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { happy_var_2 -> 
	case happyOut62 happy_x_4 of { happy_var_4 -> 
	happyIn65
		 (AbsMajaC.ITup happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_108 = happySpecReduce_1  29# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn66
		 (AbsMajaC.CInt happy_var_1
	)}

happyReduce_109 = happySpecReduce_1  29# happyReduction_109
happyReduction_109 happy_x_1
	 =  happyIn66
		 (AbsMajaC.CTrue
	)

happyReduce_110 = happySpecReduce_1  29# happyReduction_110
happyReduction_110 happy_x_1
	 =  happyIn66
		 (AbsMajaC.CFalse
	)

happyReduce_111 = happySpecReduce_1  29# happyReduction_111
happyReduction_111 happy_x_1
	 =  happyIn66
		 (AbsMajaC.CTrueB
	)

happyReduce_112 = happySpecReduce_1  29# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn66
		 (AbsMajaC.CFalseB
	)

happyReduce_113 = happySpecReduce_1  30# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn67
		 (AbsMajaC.Negative
	)

happyReduce_114 = happySpecReduce_1  30# happyReduction_114
happyReduction_114 happy_x_1
	 =  happyIn67
		 (AbsMajaC.LogNeg
	)

happyReduce_115 = happySpecReduce_1  31# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn68
		 (AbsMajaC.TInt
	)

happyReduce_116 = happySpecReduce_1  31# happyReduction_116
happyReduction_116 happy_x_1
	 =  happyIn68
		 (AbsMajaC.TBool
	)

happyReduce_117 = happySpecReduce_1  31# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	happyIn68
		 (AbsMajaC.TStruct happy_var_1
	)}

happyReduce_118 = happySpecReduce_3  31# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (AbsMajaC.TTuple happy_var_2
	)}

happyReduce_119 = happySpecReduce_2  31# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn68
		 (AbsMajaC.TRef happy_var_1
	)}

happyReduce_120 = happySpecReduce_1  31# happyReduction_120
happyReduction_120 happy_x_1
	 =  happyIn68
		 (AbsMajaC.TVoid
	)

happyReduce_121 = happySpecReduce_3  31# happyReduction_121
happyReduction_121 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (AbsMajaC.TArray happy_var_2
	)}

happyReduce_122 = happySpecReduce_1  32# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 ((:[]) happy_var_1
	)}

happyReduce_123 = happySpecReduce_3  32# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn69
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_124 = happyReduce 5# 33# happyReduction_124
happyReduction_124 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut71 happy_x_4 of { happy_var_4 -> 
	happyIn70
		 (AbsMajaC.Tag happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_125 = happySpecReduce_2  33# happyReduction_125
happyReduction_125 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn70
		 (AbsMajaC.TagType happy_var_2
	)}

happyReduce_126 = happySpecReduce_0  34# happyReduction_126
happyReduction_126  =  happyIn71
		 ([]
	)

happyReduce_127 = happySpecReduce_1  34# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn71
		 ((:[]) happy_var_1
	)}

happyReduce_128 = happySpecReduce_3  34# happyReduction_128
happyReduction_128 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_3 of { happy_var_3 -> 
	happyIn71
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_129 = happySpecReduce_2  35# happyReduction_129
happyReduction_129 happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn72
		 (AbsMajaC.StrField happy_var_1 happy_var_2
	)}}

happyReduce_130 = happySpecReduce_3  35# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn72
		 (AbsMajaC.StrFieldArr happy_var_1 happy_var_2 happy_var_3
	)}}}

happyNewToken action sts stk [] =
	happyDoAction 41# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TV happy_dollar_dollar) -> cont 39#;
	PT _ (TI happy_dollar_dollar) -> cont 40#;
	_ -> happyError' (tk:tks)
	}

happyError_ 41# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut39 x))

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut40 x))

pListStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut41 x))

pListDeclVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut42 x))

pListArrM tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut43 x))

pDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut44 x))

pDeclVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut45 x))

pDeclFun tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut46 x))

pArrM tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut47 x))

pDeclArr tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut48 x))

pStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut49 x))

pBlock tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut50 x))

pTuple tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut51 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut52 x))

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut53 x))

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut54 x))

pExp3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut55 x))

pExp4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut56 x))

pExp5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut57 x))

pExp6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut58 x))

pExp7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut59 x))

pExp8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut60 x))

pExp9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut61 x))

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut62 x))

pFuncCall tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut63 x))

pArrayInit tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut64 x))

pTupleInit tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut65 x))

pConstant tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut66 x))

pUnary_operator tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut67 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (happyOut68 x))

pListType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (happyOut69 x))

pStruct_spec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (happyOut70 x))

pListStruct_dec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (happyOut71 x))

pStruct_dec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (happyOut72 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 







-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList


















infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

