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

type Node 
  = NNum Int 
  | NAp Addr Addr
  | NGlobal Int GmCode

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
eval = error ""




