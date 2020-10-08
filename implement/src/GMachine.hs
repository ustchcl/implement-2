module GMachine where

import Language
import Parser 
import Utils

data AExpr 
  = Num Int
  | Plus AExpr AExpr
  | Mult AExpr AExpr
  | Let 
  deriving (Show)

data AInstruction
  = INum Int
  | IPlus
  | IMult
  deriving (Show)

aInterpret :: AExpr -> Int
aInterpret (Num n) = n
aInterpret (Plus e1 e2) = aInterpret e1 + aInterpret e2
aInterpret (Mult e1 e2) = aInterpret e1 * aInterpret e2

 
aEval :: ([AInstruction], [Int]) -> Int
aEval ([], []) = error "It is not a expression"
aEval ([], [n]) = n
aEval ((INum n: i), ns) = aEval (i, n:ns)
aEval (IPlus:i, (n0:n1:ns)) = aEval (i, (n1+n0):ns)
aEval (IPlus:i, _) = error "Plus was applied to too few nums"
aEval (IMult:i, (n0:n1:ns)) = aEval (i, (n1*n0):ns)
aEval (IMult:i, _) = error "Mult was applied to too few nums"

aCompile :: AExpr -> [AInstruction]
aCompile (Num n) = [INum n]
aCompile (Plus e1 e2) = aCompile e1 ++ aCompile e2 ++ [IPlus]
aCompile (Mult e1 e2) = aCompile e1 ++ aCompile e2 ++ [IMult]


-- Mark I

runProg :: String -> String
runProg = error "" -- showResults . eval . compile . parse

-- | The Basic Of GMachine
-- | Type Definitions

type GmState 
  = (GmCode,   -- current instruction stream
  GmStack,     -- current stack
  GmHeap,      -- heap of nodes
  GmGlobals,   -- global addresses in heap
  GmStats)     -- statistics

type GmCode = [Instruction]

data Instruction 
  = Unwind
  | Pushglobal Name
  | Pushint Int 
  | Push Int
  | Mkap
  | Slide Int
  deriving (Show, Eq)

getCode :: GmState -> GmCode
getCode (code, _, _, _, _) = code

putCode :: GmCode -> GmState -> GmState 
putCode newCode (_, stack, heap, globals, stats) = (newCode, stack, heap, globals, stats)

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (_, stack, _, _, _) = stack

putStack :: GmStack -> GmState -> GmState
putStack newStack (code, _, heap, globals, stats) = (code, newStack, heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (_, _, heap, _, _) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap newHeap (code, stack, heap, globals, stats) = (code, stack, newHeap, globals, stats)

data Node 
  = NNum Int 
  | NAp Addr Addr
  | NGlobal Int GmCode
  deriving (Show)

type TiGlobals = ASSOC Name Addr


getGlobals :: GmState -> GmGlobals
getGlobals (_, _, _, globals, _) = globals

putGlobals :: GmGlobals -> GmState -> GmState
putGlobals newGlobals (code, stack, heap, globals, stats) = (code, stack, heap, newGlobals, stats)

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps = (+) 1

statGetSteps :: GmStats -> Int
statGetSteps = id

getStats :: GmState -> GmStats
getStats (_, _, _, _, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats newStats (code, stack, heap, globals, stats) = (code, stack, heap, globals, newStats)

-- | Evaluator
eval :: GmState -> [GmState]
eval state = state : restStates
  where
    restStates | gmFinal state = []
               | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
              [] -> True
              otherwise -> False

step :: GmState -> GmState
step state = dispatch i (putCode is state)
  where
    (i:is) = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushGlobal f
dispatch (Pushint n) = pushint n
dispatch Mkap = mkap
dispatch (Push n) = push n
dispatch (Slice n) = slide n
dispatch Unwind = unwind

-- 将全局变量压入栈
pushGlobal :: Name -> GmState -> GmState
pushGlobal f state =
  putStack (a: getStack state) state
  where
    a = aLookup (getGlobals state) f (error $ "Undeclared global " ++ f)

-- 将int压入栈
pushint :: Int -> GmState -> GmState
pushint n state
  = putHeap heap' (putStack (a: getStack state) state)
    where
      (heap', a) = hAlloc heap (NNum n)


-- 取出栈顶的两个元素, 构造一个NAp
mkap :: GmState -> GmState
mkap state
  = putHeap heap' (putStack (a:as') state)
    where 
      (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
      (a1:a2:as') = getStack state

-- 取出栈上第N+1个元素(NAp 元素), 将其参数压入栈
push :: Int -> GmState -> GmState
push n state
  = putStack (a:as) state
    where
      as = getState stack
      a = getArg (hLookup (getHeap state) (as !! (n+1)))

getArg :: Node -> Addr
getArg (NAp _ a) = a
getArg node = error $ "This node is not a NAp" ++ show node

-- slide 移除栈顶1..n的元素
slide :: Int -> GmState -> GmState 
slide n state 
  = putStack (a : drop n as) state
    where 
      (a:as) = getStack stack

-- 
unwind :: GmState -> GmState
unwind state
  = newState (hLooup heap a)
    where
      (a:as) = getStack state
      heap = getHeap state

      newState (NNum n) = state 
      newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state
      newState (NGlobal n c)
        | length as < n = error "Unwinding with too few arguments"
        | otherwise = putCode c state

compile :: CoreProgram -> GmState
compile program = (initialCode, [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
  = mapAccuml allocateSc hInitial compiled
    where 
      -- compiled = compileSc <$> (preludeDefs ++ program) ++ compiledPrimitives
      compiled = compileSc <$> program

type GmCompiledSc = (Name, Int, GmCode)

allocateSc :: GmCode
allocateSc heap (name, nargs, instns)
  = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSc
compileSc (name, env, body) 
  = (name, length env, compileR body (zip env [0..]))
  
compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler
compileC (EVar v) env
  | v `elem` (aDomain env) = [Push n]
  | otherwise = [Pushglobal v]
  where
    n = aLookup env v (error "Can't happen")
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env 
  = compileC e2 env ++ 
    compileC e1 (argOffset 1 env) ++
    [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]


