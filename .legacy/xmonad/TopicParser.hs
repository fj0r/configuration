{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module TopicParser where

import Text.Parsec hiding (Line, (<|>), label, many)
import Text.Parsec.Language
import Text.Parsec.Token
import Control.Applicative
import Data.Monoid
import Language.Haskell.TH.Lift
{--
format:
 <topic-name> (/ <directory>)? (-> <program>)?
--}

type TopicName = String
type LineNumber = Int
newtype Labeled = Labeled {unLabel :: (String, LineNumber)}
        deriving Show
newtype Program = Program {unProgram :: [(LineNumber,  Instruction)] }
        deriving ( Show, Eq)

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
     deriving (Eq, Show)

data Opcode =
            -- * Branching
             If -- If s1, then s2 else s3
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
           | Drop -- drop(s1) -> drop s1 from the stack
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

$(deriveLiftMany [''Line,''Program,''Instruction,''Opcode])

data UserState = US {
     programLine :: Int,
     programLabels :: [Labeled]
   } deriving Show

startUserState :: UserState
startUserState = US 1 []

type Parser a = Parsec String UserState a

testParser :: Parser a -> String -> Either ParseError a
testParser pars = runParser pars startUserState "test"

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return (runParser p startUserState fname input)
lineLanguage :: LanguageDef UserState
lineLanguage = LanguageDef {
             commentStart = "{-",
             commentEnd = "-}",
             commentLine = "--",
             nestedComments = True,
             identStart = identifierStart,
             identLetter = identifierLetter,
             opStart = fail "no operators",
             opLetter = fail "no operators",
             reservedNames = [
                  "if",
                  "ffi",
                  "un",
                  "nop",
                  "app",
                  "cal",
                  "ret",
                  "jmp",
                  "sz",
                  "snz",
                  "jz",
                  "jnz",
                  "and",
                  "or",
                  "xor",
                  "not",
                  "add",
                  "sub",
                  "mul",
                  "eq",
                  "abs",
                  "sgn",
                  "exp",
                  "dec",
                  "inc",
                  "dup",
                  "ovr",
                  "swp",
                  "und",
                  "pop",
                  "rt3",
                  "rt4",
                  "rt5",
                  "drp",
                  "#t",
                  "#f",
                  "ext",
                  "#s0",
                  "#s1",
                  "cst",
                  "lod",
                  "sto",
                  "eq"
             ],
             reservedOpNames = [],
             caseSensitive = True


       }

lineTokenParser :: TokenParser UserState
lineTokenParser = makeTokenParser lineLanguage

identifierStart :: Parser Char
identifierStart = oneOf ['a' .. 'z']

identifierLetter :: Parser Char
identifierLetter = oneOf (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_/|")

topicDescriptor :: Parser [Line]
topicDescriptor = many line

line :: Parser Line
line = do
     nm <- topicName
     fp <- optionMaybe  filePath
     prg <- optionMaybe program
     return $ Line nm fp prg

topicName :: Parser TopicName
topicName = lineIdentifier

filePath :: Parser FilePath
filePath = (char '/' *> lineWhitespace) *> lineString <?> "filepath should be a string"

program :: Parser Program
program = (string "->" *> lineWhitespace) *> lineInBrackets (Program  <$> many instruction) <?> "invalid program"

instruction :: Parser (LineNumber, Instruction)
instruction = do
         pp <- getState
         res <- label <|> jumps <|> values <|> ffi <|> opcodes
         modifyState (\pp -> pp {programLine = programLine pp + 1})
         return (programLine pp, res)

-- | Value parsers
values :: Parser Instruction
values = stringd <|> identifierd <|> integerd <|> doubled <|> boold <|> blockd
stringd,identifierd, integerd,doubled,boold,blockd :: Parser Instruction
identifierd = Identifier <$> lineIdentifier
stringd = String <$> lineString
integerd = Integer <$> lineInteger
doubled = Double <$> lineFloat
boold = lineReserved "#t" *> pure (Bool True) <|> lineReserved "#f" *> pure (Bool False)
blockd = do
       blkc <- lineInBrackets (modifyState (\pp -> pp {programLine = programLine pp + 1}) *> ( Program <$> many instruction))
       lbl <- char ':' *> lineIdentifier
       return $ BlockCode lbl blkc

-- | Al the opcodes
opcodes :: Parser Instruction
opcodes = branching <|> nop <|> apply <|> functional <|> logical <|> stack <|> meta_stack <|> memory <|> exit_flow

-- | Flow oriented opcodes
jumps :: Parser Instruction
jumps = jmp <|> jz <|> jnz
jz,jnz,jmp :: Parser Instruction
jmp = lineReservedJump "jmp" Jmp
jz = lineReservedJump "jz" JumpZero
jnz = lineReservedJump "jnz" JumpZero

-- | Branching
branching :: Parser Instruction
branching = ifd <|> unlessd <|> skipz <|> skipnz

skipz, skipnz, unlessd, ifd :: Parser Instruction
ifd = lineReservedOpcode "if" If
unlessd = lineReservedOpcode "un" Unless
skipz = lineReservedOpcode "sz" SkipZero
skipnz = lineReservedOpcode "snz" SkipNotZero

-- | Do nothing
nop :: Parser Instruction
nop = lineReservedOpcode "nop" Nop

-- | Block code
apply :: Parser Instruction
apply = lineReservedOpcode "app" Apply

-- | Function oriented
functional :: Parser Instruction
functional = call <|> returnd
returnd, call :: Parser Instruction
call = lineReservedOpcode "cal" Call
returnd = lineReservedOpcode "ret" Return

-- | Logical
logical :: Parser Instruction
logical = add <|> sub <|> mul <|> absd <|> sgnd <|> expd <|> dec
        <|> inc <|> eq
eq,add,sub,mul,absd,sgnd,expd,dec,inc :: Parser Instruction
add = lineReservedOpcode "add" Add
sub = lineReservedOpcode "sub" Sub
mul = lineReservedOpcode "mul" Mul
absd = lineReservedOpcode "abs" Abs
sgnd = lineReservedOpcode "sgn" Sgn
expd = lineReservedOpcode "exp" Exp
dec = lineReservedOpcode "dec" Dec
inc = lineReservedOpcode "inc" Inc
eq = lineReservedOpcode "eq" Eq

-- | Stack manipulation
stack :: Parser Instruction
stack = dup <|> over
     <|> swp <|> under <|> pop <|> rot3
     <|> rot4 <|> rot5 <|>
     dropd
dup,over,swp,under,pop,rot3,rot4,rot5,dropd :: Parser Instruction
dup = lineReservedOpcode "dup" Dup
over = lineReservedOpcode "ovr" Over
swp = lineReservedOpcode "swp" Swp
under = lineReservedOpcode "und" Under
pop = lineReservedOpcode "pop" Pop
rot3 = lineReservedOpcode "rt3" Rot3
rot4 = lineReservedOpcode "rt4" Rot4
rot5 = lineReservedOpcode "rt5" Rot5
dropd = lineReservedOpcode "drp" Drop

-- | Meta stack manipulation
meta_stack :: Parser Instruction
meta_stack = lineReservedOpcode "#s0" SOff <|> lineReservedOpcode "#s1" SOn <|> create_stack

create_stack :: Parser Instruction
create_stack = lineReservedOpcode "cst" Cst

-- | Memory manipulation
memory :: Parser Instruction
memory = lineReservedOpcode "lod" Lod <|> lineReservedOpcode "sto" Stor

-- | Exit flow
exit_flow :: Parser Instruction
exit_flow = lineReservedOpcode "ext" Exit
-- | Ffi calls (an ffi call can do anything to the stack)

ffi = lineReserved "ffi" *> (ForeignCall <$> lineIdentifier)


lineInBrackets = brackets lineTokenParser
lineReserved = reserved lineTokenParser
lineReservedOpcode x y = reserved lineTokenParser x *> opcode y
lineReservedJump x y = reserved lineTokenParser x *> (jump y <$> lineIdentifier)
lineWhitespace = whiteSpace lineTokenParser
lineString = stringLiteral lineTokenParser
lineIdentifier = identifier lineTokenParser
lineInteger = integer lineTokenParser
lineFloat = float lineTokenParser

opcode = pure . Opcode
jump n = Jump n

label :: Parser Instruction
label  = string ":" *> (Label <$> identifier lineTokenParser)
