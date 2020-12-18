{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
module TopicOptimizer where

import TopicParser hiding (logical)
import TopicMachine

import qualified Data.Vector as V
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Except
import Data.List hiding (any)
import qualified Data.Set as S
import Data.Monoid
import Prelude hiding (any)


-- * Tail call elimimation
{--

The idea behind tce is that we can replace self-calls (which store return frames) with unconditional jumps (which don't store frames),
if no operation is done after a self-call. The reason for this, is that the return frame, doesn't really carry information. It is just
a chain of empty operations, until we hit the original caller, which called the function.

By replacing the call with a jump, we keep the interpreter from getting clogged up with frames.

The principle can be extended to multiple functions. The transformation is probably safe in the general case in this language, thus replacing a
call on the last place with a jump in every function. This call also don't carry information.

To do it correctly I need  to calculate the stack effects of each opcode, because these are reasonable static. There is little dynamic
stack manipulation available (drop is an exception, but is in most cases computable).
--}

renumber :: [Instruction] -> [(LineNumber, Instruction)]
renumber xs = [1..] `zip` xs

tce = undefined
testTailCall  n = do
              xs <- testProgram
              let ys = tce (vectorToProgram $ head $ TopicMachine.program xs)
              let bs = programToMachineState [("print", (\x -> liftIO (print x) >> return x))] ys
              runExceptT $ iterateN n interpreterStep bs


vectorToProgram :: V.Vector (LineNumber, Instruction) -> Program
vectorToProgram xs = Program $ V.toList xs

programToVector :: Program -> V.Vector (LineNumber, Instruction)
programToVector xs = V.fromList $ unProgram xs


{--
stack effects are reasonably analysable, because they
are pretty static. So we can calculate a lot about the
abstract stack at any moment.
Add,Sub,Mul,Exp,And,Or,Xor have all the same effects

s1     s1 `op` s2
s2 ->      s3
s3

This transformation breaks invariants of the stack (it destroys
the initial structure) and it looses information. So this
change is effectively one way. We cannot
reconstruct the stack from the right side.

Let's talk about positions. A position is a bit of information
with a place in the stack. It is precisely placed.

A position looks like this:

s 1
s 2

A transformation would look like this:

t(s 1, s 2) = (s 2, s 1)

s 1  ----> t ---> s 2
s 2               s 1

In this transformation the information, didn't get entangled with each
other. If this happens, we will loose information.

So one of the possible invariants of a stack structure, is that
the information content is kept constant during transformations.

Another invariant is the structure of the stack itself.

a >: Num >: s 1 --- show ---> b >: String >: s 1
a >: Num >: s 2               a >: Num >: s 2

the transformation show, changed the structure of the stack. It changed
the meaning of field s 1. Further more it decoupled s 1 and s 2. The
quantifiers are no longer shared. Thus if a becomes double, b will stay the same.
While in the left side, s 1 would have to become a double too, to keep structure.

Let's list the possible invariants:

* stack length
* stack order
* stack informational content

There are a couple of transformation types:
* invariant transformations
            this means the structure is preserved.
* length preserving transformations
         the length of the stack isn't changed,
         eg: rot3, rot4, rot5, swp, inc
* order preserving transformations
         the order of the elements isn't changed.
         eg: inc, dec, abs, sgn
* information preserving transformations
         the information of the stack is preserved:
         eg: swp, over, under

If order is unchanged, this implies length.
If information is unchanged, it doesn't imply length (information
may be added)

--}

-- | Invariant transformations, these
-- don't touch the stack at all
invariant :: S.Set Opcode
invariant = [Nop, Return]

lengthPreserving :: S.Set Opcode
lengthPreserving = invariant <> orderPreserving <> [
                 Swp,
                 Rot3,
                 Rot4,
                 Rot5
   ]
informationPreserving :: S.Set Opcode
informationPreserving = invariant <>
           [
           Inc,
           Dec,
           Not,
           Swp,
           Rot3,
           Rot4,
           Rot5,
           Dup,
           Over,
           Under,
           Lod
           ]
orderPreserving :: S.Set Opcode
orderPreserving = invariant <> [
                   Inc,
                   Dec,
                   Not,
                   Abs,
                   Sgn
   ]
-- | These break ordering, information and length
destructive :: S.Set Opcode
destructive = [If,Unless,Call,Add,Sub,Mul,Exp,Xor,Or,And,Pop,Stor,Eq, Drop]

-- Types
any :: S.Set Opcode
any = [Nop, Return, Eq, Dup, Over, Swp, Under, Pop, Rot3, Rot4, Rot5, Drop]
numeric :: S.Set Opcode
numeric = any <>  [Add,Sub,Mul,Exp,Abs,Sgn,Dec,Inc,Not]
integer :: S.Set Opcode
integer = numeric <> logical <> [Lod]
double :: S.Set Opcode
double = numeric
logical :: S.Set Opcode
logical = any <>  [Xor,Or,And]
integer_any :: S.Set Opcode
integer_any = [Stor]
label :: S.Set Opcode
label =  any <> [Call]
bool_code_code :: S.Set Opcode
bool_code_code =  [If, Unless]

varOp :: S.Set Opcode
varOp = [Drop]
quintOp :: S.Set Opcode
quintOp = [Rot5]
terOp :: S.Set Opcode
terOp = [Rot4]
triOp :: S.Set Opcode
triOp = [If,Unless,Rot3]
binOp :: S.Set Opcode
binOp = [Add,Sub,Mul,Exp,Xor,And,Or,Stor,Under,Over,Swp]
monOp :: S.Set Opcode
monOp = [Abs,Sgn,Lod,Not,Inc,Dec,Call, Pop]
nulOp :: S.Set Opcode
nulOp = [Pop,Nop,Return]

-- | Extended code with type information etc
-- This is an annotated mirror of the original instruction set

type Modifiers = Modifier
data Modifier where
     Arity :: Integer -> Modifier
     Type :: AbstractItem -> Modifier
     LengthInvariant :: Bool -> Modifier
     OrderInvariant :: Bool -> Modifier
     InformationInvariant :: Bool -> Modifier
     StackEffect :: (TypedItem, TypedItem) -> Modifier
  deriving Show

data ExtendedCode = EOpCode Opcode Modifiers
                  | EJump Opcode LabelName Modifiers
                  | EPush Instruction Modifiers
                  | EFFi LabelName Modifiers
                  | EBlockCode LabelName [ExtendedCode]
     deriving Show

type ExtendedProgram = [ExtendedCode]


newtype AbstractStack = AbstractStack { unAbstractStack :: [TypedItem] }
        deriving Show

-- | Keep track of the positions of effects
-- Composed means, that the information of two positions is mixed and
-- therefore probably lost.
data Position = S1 | S2 | S3
              | S4 | S5 | S6
              | S7 | S8 | S9
              | S Integer
              | Composed Position Position
     deriving Show

type TypedItem = (Quantifier, (AbstractItem, Position))


(|&&|) s x =  S.intersection s x
is_in = S.member
(|+|) a b = Composed a b
infixl 4 |+|
infixl 3 |&&|
infixr 2 `is_in`
-- |
-- identifier
-- < |
-- any -> numeric -> integer <- logical
--  |         |
-- string    double

stackEffects :: Opcode -> ([TypedItem], [TypedItem])
stackEffects c | c `is_in` logical |&&| binOp =

stackEffects c | c `is_in` destructive |&&| numeric |&&| binOp =
                 [
                 A >: anum >: S1,
                 A >: anum >: S2
                 ] >: [
                 A >: anum >: S1 |+| S2
                 ]
stackEffects c | c `is_in` numeric |&&| monOp |&&| informationPreserving =
             [ A >: anum >: S1]
             >:
             [ A >: anum >: S1]

anum= ASum AInt ADouble

alog = ASum AInt ABool
(>:) a b = (a, b)
infixr 3 >:
data Quantifier = A | B | C | D | E | F | G | H
                | X Int
     deriving Show
data AbstractItem where
     AAny :: AbstractItem
     ASum :: AbstractItem -> AbstractItem -> AbstractItem
     AId :: AbstractItem
     AInt :: AbstractItem
     AString :: AbstractItem
     ABool :: AbstractItem
     ADouble :: AbstractItem
     ABlock :: AbstractItem
            deriving Show
