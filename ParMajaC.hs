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
happyIn33 :: (Ident) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Ident)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Integer) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Integer)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Program) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Program)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([Ident]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([Ident])
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([Stmt]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ([Stmt])
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([DeclVar]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([DeclVar])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Decl) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Decl)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (DeclVar) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (DeclVar)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (DeclFun) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (DeclFun)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (DeclArr) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (DeclArr)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Stmt) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Stmt)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Block) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Block)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Exp) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Exp)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Exp) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Exp)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Exp) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Exp)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Exp) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Exp)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Exp) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Exp)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Exp) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Exp)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Exp) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Exp)
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
happyIn55 :: ([Exp]) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ([Exp])
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (ArrayInit) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (ArrayInit)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (TupleInit) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (TupleInit)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (Constant) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (Constant)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (Unary_operator) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (Unary_operator)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Type) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Type)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ([Type]) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> ([Type])
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (Struct_spec) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (Struct_spec)
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ([Struct_dec]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ([Struct_dec])
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (Struct_dec) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (Struct_dec)
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x1e\x02\x00\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x5b\x00\x23\x02\x01\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x2c\x00\x2c\x00\x01\x00\x1d\x02\x3c\x02\xbb\x03\x22\x00\x90\x00\x90\x00\x0b\x02\x90\x00\x90\x00\x19\x02\x00\x00\x07\x00\x00\x00\x16\x02\x90\x00\x00\x00\x00\x00\x17\x02\x0e\x02\x1c\x02\x00\x02\x48\x00\x00\x02\xfe\xff\x00\x02\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x00\x02\x01\x00\x00\x00\x21\x02\x03\x02\x22\x02\x02\x00\xb5\x00\xb5\x01\x73\x00\x00\x00\x33\x00\x00\x00\xfd\x01\x00\x00\x00\x00\x00\x00\x2c\x00\x01\x00\xfd\x01\x01\x00\x31\x00\xfd\x01\x13\x00\x42\x00\x0a\x01\x06\x00\x08\x00\xdf\xff\xfd\x01\xfd\x01\x00\x00\x38\x00\x00\x00\x5c\x01\x00\x00\x40\x01\xfd\x01\x00\x00\x07\x00\x90\x00\x01\x00\x1f\x02\x01\x00\xfb\x01\x07\x00\xfb\x01\x07\x00\xfb\x01\x07\x00\xfb\x01\x15\x02\x01\x02\xe7\x01\x12\x02\x17\x00\x07\x02\xe5\x01\xe5\x01\x5b\x00\x00\x00\xe6\x01\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x02\xf4\x01\xe4\x01\x01\x00\xe4\x01\xf9\x01\x0a\x00\x01\x00\x01\x00\xe2\x01\x01\x00\x01\x00\x4b\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x01\x00\xe2\x01\x01\x00\xee\x01\xc0\x00\x33\x00\x01\x00\xd0\x01\xf7\x01\x90\x00\x90\x00\xce\x01\xed\x01\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\xd9\x01\x00\x00\xec\x01\x00\x00\x00\x00\x73\x00\x73\x00\xb5\x01\xb5\x01\xb5\x01\xb5\x01\xb5\x00\xb5\x00\x02\x00\xeb\x01\x00\x00\xc3\x01\xe1\x01\xdd\x01\xdf\x01\xd8\x01\x90\x00\x01\x00\x01\x00\xc0\x01\xd2\x01\x00\x00\x00\x00\x00\x00\xca\x01\xb3\x01\xc8\x01\xbd\x01\xcd\x01\x00\x00\x00\x00\x01\x00\x00\x00\xc2\x01\x00\x00\x00\x00\xcb\x01\xa5\x01\x00\x00\x00\x00\x01\x00\xa2\x01\x71\x01\x00\x00\x01\x00\x00\x00\x00\x00\x94\x01\x00\x00\x00\x00\x78\x01\x00\x00\x3f\x00\x5b\x00\x61\x01\x01\x00\x6c\x01\x00\x00\x42\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xba\x01\xa1\x00\x64\x01\x66\x01\x80\x01\x31\x01\x9b\x01\x65\x01\x94\x00\x53\x01\xf8\x02\x13\x03\x3f\x03\x61\x03\x82\x03\xae\x03\xc4\x03\x01\x01\x51\x01\x74\x00\x12\x01\x3c\x01\x18\x01\x10\x00\x15\x01\x87\x01\xe1\x00\x26\x01\x9c\x01\xf5\xff\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x02\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x01\xc2\x02\x00\x00\xa7\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8b\x00\x3b\x00\x8c\x02\x00\x00\x71\x02\x00\x00\x80\x00\x00\x00\x72\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x88\x00\x00\x00\x6c\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x56\x02\x49\x00\x00\x00\x00\x00\x3b\x02\x20\x02\x4f\x00\x05\x02\xea\x01\x88\x00\x29\x03\x4b\x03\x77\x03\x6c\x03\xa3\x03\x98\x03\x8d\x03\x55\x03\xb9\x03\x1c\x03\xe6\x00\xcb\x00\xdc\x00\x45\x00\xcf\x01\x00\x00\x00\x00\x00\x00\xc1\x00\x00\x00\x00\x00\x8e\x00\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x16\x01\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x00\xb4\x01\x99\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x01\x00\x00\x00\x00\x00\x00\x48\x01\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x88\x00\x7c\x00\x12\x00\x2d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdc\xff\x00\x00\xdc\xff\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x00\x00\x00\x00\xe1\xff\x00\x00\x98\xff\x00\x00\x00\x00\x99\xff\x9a\xff\x00\x00\x00\x00\x90\xff\x00\x00\x95\xff\x00\x00\x00\x00\x00\x00\x9b\xff\x9c\xff\xa1\xff\x00\x00\x9d\xff\x9e\xff\x9f\xff\xa0\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xa6\xff\xa9\xff\xa5\xff\xc2\xff\xc0\xff\xbe\xff\xbb\xff\xb6\xff\xb3\xff\xb0\xff\xae\xff\xaa\xff\x00\x00\xc4\xff\xc3\xff\xa8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\x00\x00\xcd\xff\x00\x00\xd6\xff\x00\x00\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\xde\xff\x00\x00\x00\x00\xdf\xff\xdb\xff\x00\x00\xda\xff\xd5\xff\xd7\xff\xd4\xff\x96\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xaf\xff\xa6\xff\x00\x00\x00\x00\x00\x00\x91\xff\x92\xff\x00\x00\x8e\xff\x97\xff\x91\xff\x8f\xff\x94\xff\xa6\xff\xa3\xff\xa4\xff\xa7\xff\x00\x00\xab\xff\x00\x00\xb1\xff\xb2\xff\xb4\xff\xb5\xff\xb7\xff\xb9\xff\xb8\xff\xba\xff\xbd\xff\xbc\xff\xbf\xff\xc1\xff\xc5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xca\xff\x00\x00\xc7\xff\xd8\xff\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xcb\xff\x00\x00\xd1\xff\x00\x00\xad\xff\xac\xff\x00\x00\x00\x00\x93\xff\xa2\xff\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\xc9\xff\xc6\xff\x00\x00\xdc\xff\xd0\xff\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\x00\x00\xd3\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x24\x00\x02\x00\x01\x00\x05\x00\x28\x00\x02\x00\x05\x00\x03\x00\x0a\x00\x04\x00\x04\x00\x0a\x00\x05\x00\x1b\x00\x01\x00\x1d\x00\x11\x00\x1f\x00\x14\x00\x15\x00\x11\x00\x14\x00\x15\x00\x07\x00\x1a\x00\x05\x00\x0b\x00\x1a\x00\x0c\x00\x16\x00\x0b\x00\x21\x00\x01\x00\x23\x00\x21\x00\x28\x00\x26\x00\x27\x00\x19\x00\x26\x00\x27\x00\x0a\x00\x26\x00\x28\x00\x18\x00\x28\x00\x05\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x05\x00\x20\x00\x05\x00\x22\x00\x23\x00\x28\x00\x0b\x00\x26\x00\x0b\x00\x28\x00\x14\x00\x15\x00\x07\x00\x0b\x00\x05\x00\x00\x00\x1a\x00\x16\x00\x10\x00\x16\x00\x08\x00\x03\x00\x0a\x00\x21\x00\x16\x00\x00\x00\x05\x00\x09\x00\x26\x00\x27\x00\x0b\x00\x0b\x00\x1b\x00\x18\x00\x1d\x00\x28\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x05\x00\x22\x00\x23\x00\x18\x00\x00\x00\x26\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x28\x00\x20\x00\x00\x00\x22\x00\x23\x00\x03\x00\x25\x00\x26\x00\x00\x00\x18\x00\x00\x00\x01\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x07\x00\x20\x00\x00\x00\x22\x00\x23\x00\x0c\x00\x00\x00\x26\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x15\x00\x05\x00\x00\x00\x07\x00\x19\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x05\x00\x05\x00\x1b\x00\x07\x00\x1d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x1b\x00\x00\x00\x1d\x00\x1b\x00\x03\x00\x1d\x00\x00\x00\x01\x00\x18\x00\x1b\x00\x1c\x00\x1d\x00\x1b\x00\x1d\x00\x1d\x00\x1b\x00\x20\x00\x1d\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x0e\x00\x0f\x00\x00\x00\x06\x00\x12\x00\x13\x00\x09\x00\x00\x00\x00\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x1b\x00\x1c\x00\x1d\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x1b\x00\x1c\x00\x1d\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x04\x00\x0e\x00\x0f\x00\x19\x00\x1a\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x1a\x00\x18\x00\x1b\x00\x28\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x1d\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x14\x00\x15\x00\x1b\x00\x0d\x00\x1d\x00\x19\x00\x10\x00\x00\x00\x01\x00\x17\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0b\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x14\x00\x15\x00\x25\x00\x04\x00\x0d\x00\x19\x00\x05\x00\x10\x00\x07\x00\x09\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0d\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x1b\x00\x1b\x00\x1d\x00\x1d\x00\x23\x00\x0d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x23\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x1b\x00\x1b\x00\x1d\x00\x1d\x00\x1e\x00\x1f\x00\x0d\x00\x1b\x00\x08\x00\x1d\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0d\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x1b\x00\x1b\x00\x1d\x00\x1d\x00\x1e\x00\x1f\x00\x02\x00\x08\x00\x04\x00\x0a\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x06\x00\x10\x00\x06\x00\x17\x00\x0d\x00\x23\x00\x0d\x00\x06\x00\x19\x00\x17\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0d\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x0d\x00\x10\x00\x0d\x00\x04\x00\x17\x00\x23\x00\x06\x00\x06\x00\x06\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x23\x00\x26\x00\x10\x00\x16\x00\x05\x00\x26\x00\x28\x00\x0d\x00\x28\x00\x09\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x0d\x00\x28\x00\x05\x00\x28\x00\x04\x00\x24\x00\x28\x00\x0d\x00\x09\x00\x20\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x28\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x26\x00\x28\x00\x26\x00\x23\x00\x05\x00\xff\xff\xff\xff\x26\x00\xff\xff\x23\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x19\x00\x1a\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\x00\x00\x01\x00\x19\x00\x1a\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x14\x00\x15\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x1a\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\x21\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x7c\x00\x2f\x00\x8a\x00\x8c\x00\x2f\x00\x4c\x00\xff\xff\x8c\x00\x4e\x00\x7c\x00\x30\x00\x8b\x00\xe4\x00\x30\x00\xc1\x00\x20\x00\x30\x00\x21\x00\x8d\x00\x22\x00\x33\x00\x34\x00\x8d\x00\x33\x00\x34\x00\x94\x00\x35\x00\x24\x00\xe9\x00\x35\x00\x95\x00\xc2\x00\xdd\x00\x36\x00\x2f\x00\x3b\x00\x36\x00\xff\xff\x20\x00\x37\x00\x31\x00\x20\x00\x37\x00\x30\x00\x20\x00\xff\xff\x25\x00\xff\xff\x4e\x00\x62\x00\x63\x00\x26\x00\x64\x00\x96\x00\x27\x00\x96\x00\x65\x00\x59\x00\xff\xff\x97\x00\x20\x00\x97\x00\xff\xff\x33\x00\x34\x00\x81\x00\x86\x00\x24\x00\xac\x00\x35\x00\x98\x00\x87\x00\x98\x00\x92\x00\x7c\x00\x93\x00\x36\x00\x88\x00\xbd\x00\x24\x00\x9f\x00\x20\x00\x37\x00\xc3\x00\xc5\x00\x6a\x00\x25\x00\x21\x00\xff\xff\x62\x00\x63\x00\x26\x00\x64\x00\xe8\x00\x27\x00\x24\x00\x65\x00\x59\x00\x25\x00\x7a\x00\x20\x00\x62\x00\x63\x00\x26\x00\x64\x00\xff\xff\x27\x00\x71\x00\x65\x00\x59\x00\xc7\x00\xbb\x00\x20\x00\x7c\x00\x25\x00\x3b\x00\x30\x00\x62\x00\x63\x00\x26\x00\x64\x00\x94\x00\x27\x00\x59\x00\x65\x00\x59\x00\x95\x00\x7d\x00\x20\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\xe6\x00\x5f\x00\x59\x00\x4c\x00\xcc\x00\x82\x00\x6f\x00\x49\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x75\x00\x5f\x00\x59\x00\x24\x00\xc6\x00\x60\x00\x6f\x00\x21\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x6a\x00\x71\x00\x21\x00\x60\x00\x72\x00\x21\x00\x3b\x00\x30\x00\x25\x00\x2a\x00\xa6\x00\x21\x00\x6a\x00\x26\x00\x21\x00\x60\x00\x27\x00\x21\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xd4\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x8e\x00\x8f\x00\xa0\x00\xab\x00\x90\x00\x91\x00\xa8\x00\xa2\x00\x3b\x00\x30\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xa9\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\xae\x00\x44\x00\x45\x00\x2a\x00\xa1\x00\x21\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xad\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\xaf\x00\x44\x00\x45\x00\x2a\x00\x2b\x00\x21\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x9c\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x4f\x00\x44\x00\x45\x00\x88\x00\x8e\x00\x8f\x00\x49\x00\x4a\x00\x90\x00\x91\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x2d\x00\x37\x00\x20\x00\xff\xff\x21\x00\xd5\x00\x28\x00\x3b\x00\x30\x00\x69\x00\xe8\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x29\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x9a\x00\x45\x00\x6a\x00\x79\x00\x21\x00\x49\x00\x84\x00\x3b\x00\x30\x00\x39\x00\xdf\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x57\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x4e\x00\x45\x00\xec\x00\x70\x00\x7a\x00\x49\x00\x6e\x00\x85\x00\x6f\x00\x65\x00\xe2\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xeb\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x66\x00\x6a\x00\x21\x00\x21\x00\x59\x00\xe4\x00\x6b\x00\x6c\x00\x5c\x00\x6d\x00\xd9\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xe1\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x60\x00\x20\x00\x21\x00\x21\x00\xa5\x00\x28\x00\xe6\x00\x2c\x00\x67\x00\x21\x00\xca\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xe2\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x68\x00\x20\x00\x21\x00\x21\x00\x27\x00\x28\x00\x73\x00\x92\x00\x74\x00\x93\x00\xcb\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xd7\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\xd8\x00\xd9\x00\xdb\x00\xdc\x00\xdd\x00\x59\x00\xdf\x00\xc9\x00\xca\x00\xd2\x00\xab\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xce\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\xcf\x00\xd0\x00\xd1\x00\x8b\x00\xd4\x00\xa5\x00\xd3\x00\xa4\x00\xab\x00\xa9\x00\xbb\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xa8\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x59\x00\x20\x00\xc3\x00\xc2\x00\xc1\x00\x20\x00\xff\xff\x79\x00\xff\xff\x77\x00\xbc\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x78\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x7a\x00\xff\xff\x80\x00\xff\xff\x8b\x00\x8a\x00\xff\xff\xa0\x00\x9c\x00\x27\x00\xbe\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\xff\xff\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x20\x00\xff\xff\x20\x00\x3b\x00\x39\x00\x00\x00\x00\x00\x20\x00\x00\x00\x59\x00\xbf\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x30\x00\x00\x00\x00\x00\x55\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\xb0\x00\x43\x00\x44\x00\x45\x00\x00\x00\x00\x00\x00\x00\x49\x00\x4a\x00\xb9\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x30\x00\x54\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\xb8\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\xb2\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x3b\x00\x30\x00\x49\x00\x4a\x00\x53\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\xb6\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\xb7\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x52\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\xb3\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\xb4\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\xb5\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x51\x00\x42\x00\x43\x00\x44\x00\x45\x00\x3b\x00\x30\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x00\x00\xb1\x00\x43\x00\x44\x00\x45\x00\x33\x00\x34\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x35\x00\x50\x00\x43\x00\x44\x00\x45\x00\x00\x00\x00\x00\x36\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (30, 113) [
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
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
	(113 , happyReduce_113)
	]

happy_n_terms = 41 :: Int
happy_n_nonterms = 32 :: Int

happyReduce_30 = happySpecReduce_1  0# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn33
		 (Ident happy_var_1
	)}

happyReduce_31 = happySpecReduce_1  1# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn34
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_32 = happySpecReduce_1  2# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (AbsMajaC.Prog (reverse happy_var_1)
	)}

happyReduce_33 = happySpecReduce_1  3# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  3# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_0  4# happyReduction_35
happyReduction_35  =  happyIn37
		 ([]
	)

happyReduce_36 = happySpecReduce_2  4# happyReduction_36
happyReduction_36 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_37 = happySpecReduce_0  5# happyReduction_37
happyReduction_37  =  happyIn38
		 ([]
	)

happyReduce_38 = happySpecReduce_1  5# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_39 = happySpecReduce_3  5# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_40 = happySpecReduce_2  6# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsMajaC.DeclV happy_var_1
	)}

happyReduce_41 = happySpecReduce_1  6# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsMajaC.DeclF happy_var_1
	)}

happyReduce_42 = happySpecReduce_2  6# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (AbsMajaC.DeclA happy_var_1
	)}

happyReduce_43 = happySpecReduce_2  7# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (AbsMajaC.DVar happy_var_1 happy_var_2
	)}}

happyReduce_44 = happyReduce 11# 8# happyReduction_44
happyReduction_44 (happy_x_11 `HappyStk`
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
	 = case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	case happyOut37 happy_x_7 of { happy_var_7 -> 
	case happyOut45 happy_x_9 of { happy_var_9 -> 
	happyIn41
		 (AbsMajaC.DFun happy_var_1 happy_var_2 happy_var_4 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest}}}}}

happyReduce_45 = happyReduce 5# 9# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut45 happy_x_4 of { happy_var_4 -> 
	happyIn42
		 (AbsMajaC.DArr happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_46 = happyReduce 4# 10# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_47 = happyReduce 6# 10# happyReduction_47
happyReduction_47 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	case happyOut45 happy_x_5 of { happy_var_5 -> 
	happyIn43
		 (AbsMajaC.SAssignS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_48 = happyReduce 7# 10# happyReduction_48
happyReduction_48 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut45 happy_x_6 of { happy_var_6 -> 
	happyIn43
		 (AbsMajaC.SAssignA happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_1  10# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (AbsMajaC.SBlock happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  10# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (AbsMajaC.SDeclF happy_var_1
	)}

happyReduce_51 = happyReduce 4# 10# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SDeclA happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_52 = happyReduce 4# 10# happyReduction_52
happyReduction_52 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SDeclV happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_53 = happySpecReduce_3  10# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SIf happy_var_2 happy_var_3
	)}}

happyReduce_54 = happyReduce 5# 10# happyReduction_54
happyReduction_54 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn43
		 (AbsMajaC.SIfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_55 = happyReduce 9# 10# happyReduction_55
happyReduction_55 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut45 happy_x_4 of { happy_var_4 -> 
	case happyOut45 happy_x_6 of { happy_var_6 -> 
	case happyOut43 happy_x_8 of { happy_var_8 -> 
	case happyOut44 happy_x_9 of { happy_var_9 -> 
	happyIn43
		 (AbsMajaC.SFor happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_9
	) `HappyStk` happyRest}}}}}

happyReduce_56 = happySpecReduce_3  10# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SWhile happy_var_2 happy_var_3
	)}}

happyReduce_57 = happyReduce 5# 10# happyReduction_57
happyReduction_57 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsMajaC.SPrint happy_var_3
	) `HappyStk` happyRest}

happyReduce_58 = happySpecReduce_3  11# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (AbsMajaC.SBl (reverse happy_var_2)
	)}

happyReduce_59 = happySpecReduce_1  12# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (AbsMajaC.EIArr happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  12# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (AbsMajaC.EITup happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  12# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_62 = happySpecReduce_3  13# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (AbsMajaC.EOr happy_var_1 happy_var_3
	)}}

happyReduce_63 = happySpecReduce_1  13# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_64 = happySpecReduce_3  14# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (AbsMajaC.EAnd happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_1  14# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_66 = happySpecReduce_3  15# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsMajaC.EEq happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_3  15# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsMajaC.ENeq happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_1  15# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  16# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.ELt happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  16# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.EGt happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_3  16# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.ELe happy_var_1 happy_var_3
	)}}

happyReduce_72 = happySpecReduce_3  16# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (AbsMajaC.EGe happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_1  16# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  17# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (AbsMajaC.EAdd happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_3  17# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (AbsMajaC.ESub happy_var_1 happy_var_3
	)}}

happyReduce_76 = happySpecReduce_1  17# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  18# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (AbsMajaC.EMul happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_3  18# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (AbsMajaC.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_1  18# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_2  19# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (AbsMajaC.EPreop happy_var_1 happy_var_2
	)}}

happyReduce_81 = happySpecReduce_1  19# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_82 = happyReduce 4# 20# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMajaC.EFunkpar happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_83 = happyReduce 4# 20# happyReduction_83
happyReduction_83 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMajaC.EArray happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_84 = happySpecReduce_3  20# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (AbsMajaC.ESelect happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  20# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  21# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (AbsMajaC.EVar happy_var_1
	)}

happyReduce_87 = happySpecReduce_1  21# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (AbsMajaC.EConst happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  21# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn54
		 (happy_var_2
	)}

happyReduce_89 = happySpecReduce_0  22# happyReduction_89
happyReduction_89  =  happyIn55
		 ([]
	)

happyReduce_90 = happySpecReduce_1  22# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 ((:[]) happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  22# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_3  23# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn56
		 (AbsMajaC.IArr happy_var_2
	)}

happyReduce_93 = happyReduce 5# 24# happyReduction_93
happyReduction_93 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut55 happy_x_4 of { happy_var_4 -> 
	happyIn57
		 (AbsMajaC.ITup happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_94 = happySpecReduce_1  25# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (AbsMajaC.CInt happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  25# happyReduction_95
happyReduction_95 happy_x_1
	 =  happyIn58
		 (AbsMajaC.CTrue
	)

happyReduce_96 = happySpecReduce_1  25# happyReduction_96
happyReduction_96 happy_x_1
	 =  happyIn58
		 (AbsMajaC.CFalse
	)

happyReduce_97 = happySpecReduce_1  25# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn58
		 (AbsMajaC.CTrueB
	)

happyReduce_98 = happySpecReduce_1  25# happyReduction_98
happyReduction_98 happy_x_1
	 =  happyIn58
		 (AbsMajaC.CFalseB
	)

happyReduce_99 = happySpecReduce_1  26# happyReduction_99
happyReduction_99 happy_x_1
	 =  happyIn59
		 (AbsMajaC.Negative
	)

happyReduce_100 = happySpecReduce_1  26# happyReduction_100
happyReduction_100 happy_x_1
	 =  happyIn59
		 (AbsMajaC.LogNeg
	)

happyReduce_101 = happySpecReduce_1  27# happyReduction_101
happyReduction_101 happy_x_1
	 =  happyIn60
		 (AbsMajaC.TInt
	)

happyReduce_102 = happySpecReduce_1  27# happyReduction_102
happyReduction_102 happy_x_1
	 =  happyIn60
		 (AbsMajaC.TBool
	)

happyReduce_103 = happySpecReduce_1  27# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (AbsMajaC.TStruct happy_var_1
	)}

happyReduce_104 = happySpecReduce_3  27# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn60
		 (AbsMajaC.TTuple happy_var_2
	)}

happyReduce_105 = happySpecReduce_2  27# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (AbsMajaC.TRef happy_var_1
	)}

happyReduce_106 = happySpecReduce_1  28# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ((:[]) happy_var_1
	)}

happyReduce_107 = happySpecReduce_3  28# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_3 of { happy_var_3 -> 
	happyIn61
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_108 = happyReduce 5# 29# happyReduction_108
happyReduction_108 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	happyIn62
		 (AbsMajaC.Tag happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_109 = happySpecReduce_2  29# happyReduction_109
happyReduction_109 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn62
		 (AbsMajaC.TagType happy_var_2
	)}

happyReduce_110 = happySpecReduce_0  30# happyReduction_110
happyReduction_110  =  happyIn63
		 ([]
	)

happyReduce_111 = happySpecReduce_1  30# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn63
		 ((:[]) happy_var_1
	)}

happyReduce_112 = happySpecReduce_3  30# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	case happyOut63 happy_x_3 of { happy_var_3 -> 
	happyIn63
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_113 = happySpecReduce_2  31# happyReduction_113
happyReduction_113 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn64
		 (AbsMajaC.StrField happy_var_1 happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 40# notHappyAtAll action sts stk []

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
	PT _ (TV happy_dollar_dollar) -> cont 38#;
	PT _ (TI happy_dollar_dollar) -> cont 39#;
	_ -> happyError' (tk:tks)
	}

happyError_ 40# tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut35 x))

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut36 x))

pListStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut37 x))

pListDeclVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut38 x))

pDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut39 x))

pDeclVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut40 x))

pDeclFun tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut41 x))

pDeclArr tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut42 x))

pStmt tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut43 x))

pBlock tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut44 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut45 x))

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut46 x))

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut47 x))

pExp3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut48 x))

pExp4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut49 x))

pExp5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut50 x))

pExp6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut51 x))

pExp7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut52 x))

pExp8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut53 x))

pExp9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut54 x))

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut55 x))

pArrayInit tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut56 x))

pTupleInit tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut57 x))

pConstant tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut58 x))

pUnary_operator tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut59 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut60 x))

pListType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut61 x))

pStruct_spec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut62 x))

pListStruct_dec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut63 x))

pStruct_dec tks = happySomeParser where
  happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (happyOut64 x))

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

