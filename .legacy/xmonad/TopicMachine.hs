{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module TopicMachine where

{--
        We can do several optimalizations.
        TCE: this one is easy for the one block case

--}

import TopicParser hiding (stack, program, memory)
import qualified Data.HashMap.Strict as S
import qualified  Data.Vector as V
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Data.Monoid
import Data.Bits
import qualified Data.IntMap as M
import qualified Text.PrettyPrint as P

newtype Stack = Stack {
        unStack :: [Instruction]
    } deriving Show

data SymbolType = LineSymbol | BlockSymbol
     deriving Show

data Symbol = Symbol {
            label :: LabelName,
            symbolType :: SymbolType,
            symbolAddress :: Either LineNumber ProgramVector
   } deriving Show

blockSymbol lbl rv = Symbol lbl BlockSymbol (Right $ V.fromList $ unProgram rv)
lineSymbol lbl ln = Symbol lbl LineSymbol (Left ln)

type MachineContext m a = ExceptT String m a


type SymbolMap = S.HashMap LabelName Symbol
type FFIMap m = S.HashMap LabelName (MachineSate m -> MachineContext m (MachineSate m))
type ProgramVector = V.Vector (LineNumber, Instruction)

data SymbolRetrieval m = SLine LineNumber
                     | SFFi (MachineSate m -> MachineContext m (MachineSate m))
                     | SBlock ProgramVector

findLabel :: Monad m => MachineSate m-> LabelName -> MachineContext m (SymbolRetrieval m)
findLabel ms lm = case S.lookup lm (symbolTable ms) of
                  Nothing -> findFFILabel ms lm
                  Just xs -> case symbolType xs of
                                  LineSymbol -> SLine <$> left (symbolAddress xs)
                                  BlockSymbol -> SBlock <$> right (symbolAddress xs)
          where left (Left e) = return e
                left _ = errorsignal ms "findLabel left failed"
                right (Right a) = return a
                right _ = errorsignal ms "findLabel right failed"

findFFILabel :: Monad m => MachineSate m -> LabelName -> MachineContext m (SymbolRetrieval m)
findFFILabel ms lm = case S.lookup lm (ffiMap ms) of
                          Nothing -> errorsignal ms $ "Cannot find label " <> lm
                          Just x -> return $ SFFi x

-- Yes, it is called machinesate
data MachineSate m = MS {
          stack :: [Stack],
          symbolTable :: SymbolMap,
          ffiMap :: FFIMap m,
          program :: [V.Vector (LineNumber, Instruction)],
          returnStack :: [LineNumber],
          instructionPointer :: LineNumber,
          metaMode :: Bool,
          memory :: M.IntMap (Instruction)
    }

instance Show (MachineSate m) where
         show ms = show $ printMachineSate ms

printMachineSate :: MachineSate m -> P.Doc
printMachineSate ms = P.char '[' P.$$ P.nest 1 (P.text "Stack: " P.<> (printStack (head $ stack ms)) P.$$
                      P.text "SymbolTable: " P.<> (printSymbolTable (symbolTable ms)) P.$$
                      P.text "FFIMap: " P.<> (printSymbolTable (ffiMap ms)) P.$$
                      P.text "Return Stack: " P.<> ((P.text . show) $  (returnStack ms)) P.$$
                      P.text "Ip: " P.<> P.char '[' P.<+> (P.int (instructionPointer ms)) P.<+> P.char ']' P.$$
                      P.text "Instructions: " P.<> P.char '[' P.$$ P.nest 1 (
                                    printInstructions True 4 ms
                      ) P.$$ P.nest 1 (P.char ']')

                        ) P.$$ P.nest 1 (P.char ']')
            where printStack :: Stack -> P.Doc
                  printStack (Stack xs) = P.char '[' P.$$ P.nest 1 (
                                        P.vcat $ (fmap (P.hcat . P.punctuate (P.char ',')))  (part5 ((P.text . show) <$> xs))
                                   ) P.$$ P.nest 1 (P.char ']')
                  printSymbolTable (sm) = P.text (show $ S.keys sm)

printInstructions :: Bool -> Int -> MachineSate m -> P.Doc
printInstructions t n ms = let ip = instructionPointer ms
                               m = V.length (head $ program ms)
                               prg = fmap snd $ head $ program ms
                               center = prg V.! ip
                               (bl, fl) = fitWindow ip 6 m
                               back = Right <$> V.slice bl (ip - bl) prg
                               front = V.cons ( Left ()) $ (fmap Right . V.drop 1 $ V.slice (ip) (fl - ip) prg)
                           in foldr step mempty ( V.toList back <> (Right center : V.toList front))
                  where step (Right ( BlockCode lbl prg )) z = P.text "BlockCode " P.<> P.text lbl P.<+> P.char '[' P.$$
                                                   P.nest 1 (printInstructions False n (ms {instructionPointer = 0, program = return $ V.fromList $ unProgram prg} ))
                                                   P.$$ z
                        step (Left ()) z = if t then P.nest 3 (P.text "^^ --- There you are") P.$$ z else z
                        step (Right x) z = P.text ( show x) P.$$ z

{--
 0 ---c--------- n

--}
fitWindow :: Int -> Int -> Int -> (Int, Int)
fitWindow center len vlen | center < len `div` 2 = (0, right center (len - center) vlen)
                 where right center len vlen = min (vlen ) (center + len)
fitWindow center len vlen | (center + len `div` 2) > vlen   = (left center (vlen - 1 - center) vlen, vlen - 1)
                 where left center len vlen | center < len = 0
                                            | otherwise = len
fitWindow center len vlen = let (left, right) = (len `div` 2, len - left)
                            in (center - left, center + right)

part5 :: [a] -> [[a]]
part5 [] = []
part5 xs = take 5 xs : part5 (drop 5 xs)

simulateMachineState :: MachineSate m
simulateMachineState = undefined

programToMachineState :: [(LabelName, MachineSate m -> MachineContext m (MachineSate m))] -> Program -> MachineSate m
programToMachineState ffi prg =  MS ([Stack []]) (buildSymbolTable prg) (S.fromList ffi) [(V.fromList $ unProgram prg)] [] 0 False mempty

buildSymbolTable :: Program -> SymbolMap
buildSymbolTable (Program xs) = foldr step mempty xs
                 where step (l,BlockCode lbl prg) z =  S.insert lbl (blockSymbol lbl prg) z
                       step (l, Label lbl) z = S.insert lbl (lineSymbol lbl (l - 1)) z
                       step x z = z

interpreterStep :: Monad m => MachineSate m -> MachineContext m (MachineSate m)
interpreterStep ms | (instructionPointer ms) >= (V.length (head $ program ms)) = errorsignal ms "program exited unexpectedly"
interpreterStep ms = let ip = instructionPointer ms
                         stk = stack ms
                         ffim = ffiMap ms
                         smt = symbolTable ms
                         prg = head $ program ms
                         ci = prg  V.! ip
                     in case snd ci of
                          Jump opc lbl -> handleJump ms opc lbl
                          ForeignCall lbl -> do
                               xs <- findLabel ms lbl
                               case xs  of
                                   SFFi f ->  f (incInstructionPtr ms)
                                   _ -> errorsignal ms ("Label " ++ lbl ++ " should be associated by a foreing call label" )
                          Label ln -> pure $ addLabel ln (SLine ip) (incInstructionPtr ms)
                          BlockCode ln vn -> pure $ addLabel ln (SBlock ( V.fromList . unProgram $ vn)) (incInstructionPtr ms)
                          Opcode opc -> handleOpcode opc ms
                          x -> pure $ pushStack x (incInstructionPtr ms)

handleOpcode :: Monad m => Opcode -> MachineSate m -> MachineContext m (MachineSate m)
handleOpcode If ms = do ([s1,s2,s3], ms') <- (takeStack 3 ms)
                        case s1 of
                             Bool True -> (valueToSymbol ms s2) >>= \v -> (callSymbol  v (incInstructionPtr $ incInstructionPtr ms'))
                             Bool False -> (valueToSymbol ms s3) >>= \v -> ( callSymbol v (incInstructionPtr $ incInstructionPtr ms'))
                             _ -> errorsignal ms "If predicate should be of type bool"
handleOpcode Unless ms = handleOpcode If =<< (handleOpcode Not $ incInstructionPtr ms)
handleOpcode Nop ms = pure $ incInstructionPtr ms
handleOpcode Apply ms = do ([s1],ms') <- (takeStack 1 ms)
                           case s1 of
                              x@(BlockCode lbl v) -> callSymbol (SBlock (V.fromList $ unProgram v)) (incInstructionPtr ms')
                              _ -> errorsignal ms "Can only apply blocks"
handleOpcode Call ms = do ([s1], ms') <- takeStack 1 ms
                          (valueToSymbol ms s1 ) >>= \x -> (callSymbol x  (incInstructionPtr ms'))
handleOpcode Return ms = pure . popProgram =<< popInstructionPtr ms
handleOpcode SkipZero ms = do s1 <- peekStack ms
                              case s1 of
                                   Integer 0 -> pure $ incInstructionPtr (incInstructionPtr ms)
                                   Integer _ -> pure $ incInstructionPtr ms
                                   _ -> errorsignal ms "sz expects integer on s1"
handleOpcode SkipNotZero ms = do s1 <- peekStack ms
                                 case s1 of
                                         Integer 0 -> pure $ incInstructionPtr ms
                                         Integer _ -> pure $ incInstructionPtr (incInstructionPtr ms)
                                         _ -> errorsignal ms "snz expects an integer on s1"
handleOpcode And ms = handleLOp "and" (.&.) (&&) ms
handleOpcode Or ms = handleLOp "or" (.|.) (&&) ms
handleOpcode Xor ms = handleLOp "xor" xor (\a b -> a || b && not (a && b)) ms
handleOpcode Not ms =do (s1, ms') <- takeStack 1 ms
                        case head s1 of
                            Integer n -> pure $ pushStack (Integer $ negate n) $ incInstructionPtr ms'
                            Bool n -> pure $ pushStack (Bool $ not n) $ incInstructionPtr ms'
                            _ -> errorsignal ms "not expects an integer on s1"
handleOpcode Add ms = handleNOp "add" (+) (+) ms
handleOpcode Sub ms = handleNOp "sub" (-) (-) ms
handleOpcode Mul ms = handleNOp "mul" (*) (*) ms
handleOpcode Exp ms = handleNOp "exp" (^) (**) ms
handleOpcode Abs ms = handleMOp "abs" abs abs ms
handleOpcode Sgn ms = handleMOp "sgn" signum signum ms
handleOpcode Dec ms = handleMOp "dec" (+ (-1)) (+ (-1)) ms
handleOpcode Inc ms = handleMOp "inc" (+1) (+1) ms
handleOpcode Dup ms = handleStackOp 1 ms (\(x:xs) -> (x:x:xs))
handleOpcode Over ms = handleStackOp 2 ms (\(x:y:xs) -> (y:x:y:xs))
handleOpcode Swp ms = handleStackOp 2 ms (\(x:y:xs) -> (y:x:xs))
handleOpcode Under ms = handleStackOp 2 ms (\(x:y:xs) -> (x:y:x:xs))
handleOpcode Pop ms = handleStackOp 1 ms tail
handleOpcode Rot3 ms = handleStackOp 3 ms (\(x:y:z:xs) -> (y:z:x:xs))
handleOpcode Rot4 ms  = handleStackOp 4 ms (\(x:y:z:q:xs) -> (y:z:q:x:xs))
handleOpcode Rot5 ms = handleStackOp 5 ms (\(x:y:z:q:r:xs) -> (y:z:q:r:x:xs))
handleOpcode Drop ms = do amount <- peekStack ms
                          case amount of
                                Integer (fromInteger -> amount) -> handleStackOp amount ms (drop amount)
                                _ -> errorsignal ms "not expects an integer on s1"
handleOpcode Exit ms = return ms
handleOpcode SOn ms = return $ incInstructionPtr $ ms { metaMode = True}
handleOpcode SOff ms = return $ incInstructionPtr $ ms { metaMode = False}
handleOpcode Lod ms = do s1 <- peekStack ms
                         case s1 of
                           Integer sid -> case M.lookup  (fromInteger sid) (memory ms) of
                                               Nothing -> errorsignal ms $ "Nothing stored on this memory location, " ++ (show sid)
                                               Just ins -> return $ pushStack ins $ incInstructionPtr ms
                           _ -> errorsignal ms $ "lod expects a integer"
handleOpcode Stor ms = do ([s1,s2], ms') <- takeStack 2 ms
                          case s1 of
                            Integer s1 -> return $ incInstructionPtr $ ms {memory  = M.insert (fromInteger s1) s2 (memory ms) }
                            _ -> errorsignal ms "sto expects an integer as operand s1"
handleOpcode Eq ms = do ([s1,s2], ms') <- takeStack 2 ms
                        pure $ case (s1,s2) of
                                 (String x, String y) -> eqXy ms x y
                                 (Integer x, Integer y) -> eqXy ms x y
                                 (Double x, Double y) -> eqXy ms x y
                                 (Bool x, Bool y) -> eqXy ms x y
                                 (Identifier x, Identifier y) -> eqXy ms x y

eqXy ms x y = pushStack (Bool (x == y)) $ incInstructionPtr ms


handleStackOp :: Monad m => Int ->  MachineSate m -> (forall a. [a] -> [a]) -> MachineContext m (MachineSate m)
handleStackOp n ms  f1 | metaMode ms == False = stackMinLength n ms >>= \ms -> pure (withStack f1 $ incInstructionPtr ms)
                       | otherwise = do
                               if (length $ stack ms) >= n
                                  then return $ ms { stack = f1 (stack ms) }
                                  else errorsignal ms $ "Stack of stacks length is big enough: " <> (show n) <> " > " <> (show (length $ stack ms))

handleMOp x f1 f2 ms = do ([s1],ms') <- takeStack 1 ms
                          case s1 of
                                 Integer n -> pure $ pushStack (Integer $ f1 n ) $ incInstructionPtr ms'
                                 Double n -> pure $ pushStack (Double $ f2 n) $ incInstructionPtr ms'
                                 _ -> errorsignal ms $ x <> " expects integer or double on s1 and s2"


handleNOp x f1 f2 ms = do ([s1,s2],ms') <- takeStack 2 ms
                          case (s1,s2) of
                                   (Integer n, Integer p) -> pure $ pushStack (Integer $ n `f1` p) $ incInstructionPtr ms'
                                   (Double n, Double p) -> pure $ pushStack (Double $ n `f2` p) $ incInstructionPtr ms'
                                   _ -> errorsignal ms $ x <> " expects integer or double on s1 and s2"


handleLOp x f1 f2 ms = do ([s1,s2],ms') <- takeStack 2 ms
                          case (s1,s2) of
                               (Integer n, Integer p) -> pure $ pushStack (Integer $ n `f1` p) $ incInstructionPtr ms'
                               (Bool n, Bool p) -> pure $ pushStack (Bool $ n `f2` p) $ incInstructionPtr ms'
                               _ -> errorsignal ms $ x <> " expects integer or bool on s1 and s2"


valueToSymbol :: Monad m => MachineSate m -> Instruction -> MachineContext m (SymbolRetrieval m)
valueToSymbol ms (ForeignCall lbl) = do sym <- findLabel ms lbl
                                        case sym of
                                            SFFi f -> return $ SFFi f
                                            _ -> errorsignal ms $ "Expected " ++ lbl ++ " to be an FFI function"
valueToSymbol ms (Identifier lbl) = findLabel ms lbl
valueToSymbol ms (BlockCode _ v) = return $ SBlock (V.fromList $unProgram v)
valueToSymbol ms x = errorsignal ms $ "Valid symbolic computations are: Block Labels pointing to a program or an FFi call "

callSymbol :: Monad m => SymbolRetrieval m -> MachineSate m -> MachineContext m (MachineSate m)
callSymbol (SFFi f) ms = f ms
callSymbol (SBlock blk)  ms = return $ pushInstructionPtr 0 $ pushProgram blk ms
callSymbol (SLine ln) ms = return $ pushInstructionPtr ln ms


jumpSymbol :: Monad m => LabelName -> SymbolRetrieval m -> MachineSate m -> MachineContext m (MachineSate m)
jumpSymbol lbl (SFFi _) ms = errorsignal ms $ "Cannot jump into ffi " <> lbl
jumpSymbol lbl (SBlock v) ms = return $ pushProgram v $ ms {instructionPointer = 0}
jumpSymbol lbl (SLine ln) ms = return $ ms {instructionPointer = ln}

handleJump :: Monad  m => MachineSate m -> Opcode -> LabelName -> MachineContext m (MachineSate m)
handleJump ms Jmp lbl = findLabel ms lbl >>= \x -> jumpSymbol lbl x ms
handleJump ms JumpZero lbl = do
          s <- peekStack ms
          case s of
                (Integer 0) -> findLabel ms lbl >>= \x -> jumpSymbol lbl x (incInstructionPtr ms)
                (Integer n) -> return $ incInstructionPtr ms
                _ -> peekStack ms >>= \st -> errorsignal ms ( "Can only jumpzero on integers, got " <> show st )
handleJump ms JumpNotZero lbl = do
                               s <- peekStack ms
                               case s of
                                (Integer 0) -> return $ incInstructionPtr ms
                                (Integer _) -> findLabel ms lbl >>= \x -> jumpSymbol lbl x $ incInstructionPtr ms
                                _ -> peekStack ms >>= \st -> errorsignal ms ( "Can only jumpzero on integers, got " <> show st )


errorsignal :: Monad m => MachineSate m -> String -> MachineContext m a
errorsignal ms err =
               let (ln, ins) = currentInstruction ms
               in throwE $ "On line " <> (show ln) <> ": " <> err <> "\nAbstract instruction:" <> (show ins)
                                 <> "\nStack: " <>  (show (stack ms))
                                 <> "\nReturn stack: " <> (show (returnStack ms))
                                 <> "\nIp: " <> (show (instructionPointer ms))


addLabel :: LabelName -> SymbolRetrieval m -> MachineSate m -> MachineSate m
addLabel lbl (SLine ln) ms = ms {symbolTable = S.insert lbl (Symbol lbl LineSymbol (Left ln)) (symbolTable ms)}
addLabel lbl (SBlock ln) ms = ms {symbolTable = S.insert lbl (Symbol lbl BlockSymbol (Right ln)) (symbolTable ms)}
addLabel lbl (SFFi ln) ms = ms {ffiMap = S.insert lbl ln (ffiMap ms)}


pushStack :: Instruction -> MachineSate m -> MachineSate m
pushStack ins = withStack (ins:)

popStack :: Monad m => MachineSate m -> MachineContext m (MachineSate m)
popStack ms = do
       stackMinLength 1 ms
       return $ withStack tail ms

curStack :: Monad m => MachineSate m -> MachineContext m Instruction
curStack ms | null $ stacki ms = errorsignal ms "Empty stack, while asking for items"
curStack ms = return $ head $ stacki ms

takeStack :: Monad m => Int -> MachineSate m -> MachineContext m ([Instruction], MachineSate m)
takeStack n ms = do
             stackMinLength n ms
             return $ (take n $ stacki ms , withStack (drop n) ms )

peekStack :: Monad m => MachineSate m -> MachineContext m Instruction
peekStack ms = do
             stackMinLength 1 ms
             return $ head (stacki ms)

sameLength :: [a] -> [b] -> Bool
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength [] [] = True
sameLength _ _ = False

withStack :: ([Instruction] -> [Instruction]) -> MachineSate m -> MachineSate m
withStack f ms = ms {stack = fmap Stack . onFirst f . fmap unStack $ ( stack ms)}

onFirst :: (a -> a) -> [a] -> [a]
onFirst f (x:xs) = f x : xs
onFirst f [] = []

stacki :: MachineSate m  -> [Instruction]
stacki = unStack . head .  stack

stackMinLength :: Monad m => Int -> MachineSate m -> MachineContext m ( MachineSate m)
stackMinLength n ms | not ( replicate n () `sameLength` (take n $ stacki ms))  = errorsignal ms $ "Not enough elements on the stack, expected " <> (show n) <> " elements onthe stack, got " <> (show (length (stacki ms)))
stackMinLength n ms | otherwise = return ms

pushProgram :: ProgramVector -> MachineSate m ->  MachineSate m
pushProgram xs ms =  ms { program = xs : program ms}
popProgram ms = ms { program = tail (program ms)}
curProgram :: MachineSate m -> V.Vector (LineNumber, Instruction)
curProgram = head . program
pushInstructionPtr :: LineNumber -> MachineSate m -> MachineSate m
pushInstructionPtr ln ms = ms {instructionPointer = ln, returnStack = instructionPointer ms : returnStack ms}
popInstructionPtr :: Monad m => MachineSate m -> MachineContext m (MachineSate m)
popInstructionPtr ms | returnStack ms == [] = errorsignal ms "Ret called with empty return stack"
popInstructionPtr ms = return $ ms {instructionPointer = head (returnStack ms), returnStack = tail (returnStack ms)}
curInstructionPtr ms = instructionPointer $ ms
incInstructionPtr :: MachineSate m -> MachineSate m
incInstructionPtr ms = ms {instructionPointer = instructionPointer ms + 1}

currentInstruction :: MachineSate m -> (LineNumber, Instruction)
currentInstruction ms = let cip = curInstructionPtr ms
                        in (curProgram ms V.! cip)
testProgram :: IO (MachineSate IO)
testProgram = do
    (Right xs) <- parseFromFile topicDescriptor "test.tps"
    let (Line a b p) = xs !! 0
        (Just prog) = p
     in return $ programToMachineState ([("print",\xs -> (liftIO . print) ("fromProgram", xs) *> return xs )]
                                        ) prog


iterateN :: (Monad m, Applicative m) => Int -> (a -> m a) -> a -> m a
iterateN 0 f a = pure a
iterateN n f a = f a >>= \b -> iterateN (n - 1) f b

newtype RepeatM m a = RM { unRVM :: m a }

bbq :: Monad m => MachineSate m -> m (Either String (MachineSate m))
bbq ms = runExceptT $ worker ms
                  where worker ms = do res <- interpreterStep ms
                                       let ins = (currentInstruction res)
                                       case snd ins of
                                            Opcode Exit -> return res
                                            _ -> worker res
