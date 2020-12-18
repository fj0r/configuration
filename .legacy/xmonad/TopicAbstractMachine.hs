{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module TopicAbstractMachine where

import Language.Haskell.TH.Lift
import Text.PrettyPrint.HughesPJ

type TopicName = String
type LineNumber = Int

data Position = S1 | S2 | S3 | S4 | S5 | S6
              | Sa | Sb | Sc | Sd | Se | Sf
     deriving (Eq, Show)

newtype Program  = Program {unProgram :: [(LineNumber,  Instruction)] }
        deriving (Eq, Show)
type LabelName = String


data Instruction = Jump Opcode LabelName
                 | Label LabelName
                 | Integer Integer
                 | Identifier LabelName
                 | String String
                 | ForeignCall String
                 | Bool Bool
                 | Double Double
                 | Opcode Opcode
                 | BlockCode LabelName Program
                 | AnnotatedInstruction StackEffect StackEffect Instruction
     deriving (Eq, Show)

eraseAnnotation :: Instruction -> Instruction
eraseAnnotation (AnnotatedInstruction _ _ p) = p
eraseAnnotation x = x

annotateInstruction :: Instruction -> Instruction
annotateInstruction x@(Jump opc lbl) = let (i,o) = annotateOpcode opc
                                       in annInstruction i o x

annotateInstruction x@(Opcode opc) = let (i,o) = annotateOpcode opc
                                     in  annInstruction i o x
annInstruction :: StackEffect -> StackEffect -> Instruction -> Instruction
annInstruction = AnnotatedInstruction

annotateOpcode :: Opcode -> (StackEffect,StackEffect)
annotateOpcode Nop = (EmptyStack, EmptyStack)
annotateOpcode If = (
           SE S1 TBool "a" :>
            SE S2 (TOr TCode TSymbol) "b" :>
            SE S3 (TOr TCode TSymbol) "c",
           EmptyStack
    )
annotateOpcode Unless = annotateOpcode If
annotateOpcode Apply = (SE S1 TCode "a", EmptyStack)
annotateOpcode Call = (SE S1 TSymbol "a", EmptyStack)
annotateOpcode Return = (EmptyStack, EmptyStack)
annotateOpcode Jmp = (EmptyStack, EmptyStack)
annotateOpcode SkipZero = (SE S1 TInteger "a", SE S1 TInteger "a")
annotateOpcode SkipNotZero = (SE S1 TInteger "a", SE S1 TInteger "a")
annotateOpcode JumpNotZero = (SE S1 TInteger "a", SE S1 TInteger "a")
annotateOpcode JumpZero = (SE S1 TInteger "a", SE S1 TInteger "a")
annotateOpcode And = (SE S1 TLogical "a" :> SE S2 TLogical "a", SE Sa TLogical "a")
annotateOpcode Or = (SE S1 TLogical "a" :> SE S2 TLogical "a", SE Sa TLogical "a")
annotateOpcode Xor = (SE S1 TLogical "a" :> SE S2 TLogical "a", SE Sa TLogical "a")
annotateOpcode Not = (SE S1 (TOr TNumeric TLogical) "a",  SE Sa (TOr TNumeric TLogical) "a")
annotateOpcode Add = (SE S1 TNumeric "a" :> SE S2 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Sub = (SE S1 TNumeric "a" :> SE S2 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Mul  = (SE S1 TNumeric "a" :> SE S2 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Exp = (SE S1 TNumeric "a" :> SE S2 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Abs = (SE S1 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Sgn = (SE S1 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Dec = (SE S1 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Inc = (SE S1 TNumeric "a", SE Sa TNumeric "a")
annotateOpcode Eq = (SE S1 TAny "a" :> SE S2 TAny "a", SE Sa TBool "b")
annotateOpcode Dup = (
            SE S1 TAny "a" :> SE S2 TAny "b",
            SE S1 TAny "a" :> SE S1 TAny "a" :> SE S2 TAny "b"
                                       )
annotateOpcode Over = (
               tany S1 "a" :> tany S2 "b",
               tany S2 "b" :> tany S1 "a" :> tany S2 "b"
    )
annotateOpcode Swp = (
              tany S1 "a" :> tany S2 "b",
              tany S2 "b" :> tany S1 "a"
  )
annotateOpcode Under = (
             tany S1 "a" :> tany S2 "b",
             tany S1 "a" :> tany S2 "b" :> tany S1 "a"
   )
annotateOpcode Pop = (
               tany S1 "a",
               EmptyStack
   )
annotateOpcode Rot3 = (
               tany S1 "a" :> tany S2 "b" :> tany S3 "c",
               tany S2 "b" :> tany S3 "c" :> tany S1 "a"
   )
annotateOpcode Rot4 = (
               tany S1 "a" :> tany S2 "b" :> tany S3 "c" :> tany S4 "d",
               tany S2 "b" :> tany S3 "c" :> tany S4 "d" :> tany S1 "a"
   )
annotateOpcode Rot5 = (
               tany S1 "a" :> tany S2 "b" :> tany S3 "c" :> tany S4 "d" :> tany S5 "e",
               tany S2 "b" :> tany S3 "c" :> tany S4 "d" :> tany S5 "e" :> tany S1 "a"
   )
annotateOpcode Lod = (
               SE S1 TInteger "a",
               SE Sa TAny "b" :> SE S1 TInteger "a"
   )
annotateOpcode Stor = (
               SE S1 TAny "b" :> SE S1 TInteger "a",
               EmptyStack
   )
annotateOpcode Exit = (EmptyStack, EmptyStack)

tany n q = SE n TAny q
data StackEffect = SE Position Type Quantifier
                 | StackEffect :> StackEffect
                 | EmptyStack
     deriving (Eq)
instance Show StackEffect where
         show se = show $  text "---" $$ printStackEffect se $$ text "---"

instance Show (StackEffect,StackEffect) where
         show (se,sb) = show $ listStackEffect True se `exzip` listStackEffect False sb

exzip :: [Doc] -> [Doc] -> Doc
exzip (x:xs) (y:ys) = x <+> text "->" <> sepa <+> ( y) $$ exzip xs ys
exzip [] (y:ys) = mempty <+> text "->" <> sepa <+> ( y) $$ exzip [] ys
exzip (y:ys) [] =  y <+> text "->" <> sepa <+> (mempty)  $$ exzip [] ys
exzip [] [] = mempty

sepa = text (extendText 10 " ")

extendText n (x:xs) = x : extendText (n - 1) xs
extendText 0 xs = xs
extendText n [] = replicate n ' '

listStackEffect :: Bool -> StackEffect -> [Doc]
listStackEffect _ EmptyStack = []
listStackEffect b (x :> y) = listStackEffect b x ++ listStackEffect b y
listStackEffect True (SE pos tpe quant) = [ text (extendText 1 quant) <+> char ':' <+> text (extendText 0 $ show (pos)) <+> char '(' <+> text ( extendText 0 (show tpe)) <+>  text ( extendText 10 ")")]
listStackEffect False (SE pos tpe quant) = [ text quant <+> char ':' <+> text (show (pos)) <+> char '(' <+> text (show tpe) <+>  text ")"]


printStackEffect :: StackEffect -> Doc
printStackEffect EmptyStack = text "---" $$ text "---"
printStackEffect (x :> y) = printStackEffect x $$ printStackEffect y
printStackEffect (SE pos tpe quant) = text quant <+> char ':' <+> text (show (pos)) <+> char '(' <+> text (show tpe) <+> text ")"
type Quantifier = String
data Type = TAny
          | TNumeric
          | TLogical
          | TBool
          | TInteger
          | TDouble
          | TSymbol
          | TCode
          | TBottom
          | TOr Type Type
     deriving (Eq, Show)

lower TAny = [TNumeric, TSymbol, TCode, TLogical]

type a :$ b = a b

infixr 1 :$

data Opcode =
                If -- If s1, then s2 else s2
           | Unless -- Unless s1, then s3 else s2
           -- * Nop
           | Nop
           -- * Block Code calling
           | Apply
           -- * Function oriented
           | Call -- call s1 with s2, s3 .. -> r1
           | Return -- return from call
           -- * Flow oriented
           | Jmp -- Unconditional jump
           | SkipZero -- skip next instruction if s1 == zero
           | SkipNotZero -- skip next instruction if s1 == zero
           | JumpZero -- Jump to label if s1 is zero
           | JumpNotZero -- Jump to label if s1 is not zero
           -- * Logical operators
           | And -- And s1 s2 -> s1
           | Or --
           | Xor
           | Not
           -- * Numeric operations
           -- All work like:
           -- op s1 s2 -> r1
           | Add
           | Sub
           | Mul
           | Exp
           -- * Mon ops
           | Abs
           | Sgn
           | Dec
           | Inc
           -- * Value oriented
           | Eq
           -- * Stack manipulation
           | Dup -- duplicate s1, s1 s2 -> s1 s1 s2
           | Over -- pull s2 over s1, s1 s2  -> s2 s1 s2
           | Swp -- swap s1 and s2 -> s1 s2 -> s2 s1
           | Under -- put s1 under s2, s1 s2 -> s1 s2 s1
           | Pop -- pop s1, s1 s2 -> s2
           | Rot3 -- rot s1 s2 s3 -> s2 s3 s1
           | Rot4 -- rot s1 s2 s3 s4 -> s2 s3 s4 s1
           | Rot5 -- etc
           | Drop -- drop(s1) -> drop s1 from the stack TODO drop should be dropped
           -- * Meta stack manipulation
           | SOn-- Flips the stack mode bit, if flipped on, the stack of stacks can be manipulated
           | SOff -- Flips the stack mode bit, if flipped on, the stack of stacks can be manipulated
           | Cst -- Create a stack
           -- * Memory manipulation
           | Lod -- Load from s1 and push result
           | Stor -- Store s2 to s1
           -- * Flow termination
           | Exit -- Exit
     deriving (Show, Eq, Ord)

data Line where
     Line :: TopicName -> Maybe FilePath -> Maybe Program -> Line
     Comment :: String -> Line
  deriving Show

-- $(deriveLiftMany [''Line,''Program,''Instruction,''Opcode])
