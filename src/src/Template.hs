module Template where 
    
import Language
import Parser
import Utils

import Printer

{-
    A review of template instantiatiom
    1. 函数式程序通过对表达式 `求值` 执行
    2. 表达式是用图表示的
    3. 求值过程为执行一些列的化简操作
    4. 化简操作将可化简的表达式用其化简后的形式替换
    5. 如果不可再化简, 求值过程结束
    6. 每次只对一个redex(reducible expression)进行化简. 化简顺序不影响结果
    7. 选择最外层的redex进行化简, 成为 `normal order reduction`
-}

-- | 三步走
-- | 1. 找到下一个redex
-- | 2. 化简
-- | 3. 用化简后的更新原redex
-- | 两种redex
-- | 1. supercombinators
-- | 2. built-in primitives

type MultState = (Int, Int, Int, Int)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state then [state] else state : evalMult (stepMult state)
    where
        multFinal (n, m, d, t) = d == 0 && m == 0

stepMult :: MultState -> MultState
stepMult (n, m, 0, t) = (n, m-1, n, t)
stepMult (n, m, d, t) = (n, m, d-1, t+1)

-- (stack, dump, heap, global)
---   |     |      |     +---->  supercombinator or primtive的在heap的地址
---   |     |      +---------->  带有标签的节点  h[a:node] 地址a指向node   <-----------+
--    |     +----------------->  stack of stacks, 每一步化简都产生一个stack           |
--    +----------------------->  stack of addresses, heap上地址        ------------+

-- MARK I
-- rule1:
--      (a:s, d, h[a: NAp a1 a2], f)
--- ==> (a1:a:s, d, h, f)
-- rule2:
--      (a0:a1:a2:...:an:s, d, h[a0:NSupercomb [x1,...,xn] body] f
--  ==> (ar:s, d, h', f)
--       (h', ar) = instaniate body h f[x1->a1, ..., xn->an]

-- data types 

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
-- type Addr = Int

type TiDump = [TiStack]

type TiHeap = Heap Node
data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    | NInd Addr
    | NPrim Name Primitive
    deriving (Show)

data Primitive = Neg | Add | Sub | Mul | Div deriving (Show)

type TiGlobals = ASSOC Name Addr

type TiStats = Int


runProg :: String -> String
runProg = showResults . eval . compile . parse

-- compile
compile :: CoreProgram -> TiState
compile program = 
    (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        address_of_main = aLookup globals "main" (error "main is not defined")
        initial_stack = [address_of_main]

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = 
    let 
        (heap', sc_addrs) = mapAccuml allocateSc hInitial sc_defs
        (heap'', prim_addrs) = mapAccuml allocatePrim heap' primitives
    in (heap'', sc_addrs ++ prim_addrs)

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
    = (heap', (name, addr))
        where
            (heap', addr) = hAlloc heap (NSupercomb name args body)

primitives :: ASSOC Name Primitive
primitives = 
    [ ("negate", Neg)
    , ("+", Add)
    , ("-", Sub)
    , ("*", Mul)
    , ("/", Div)
    ]

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
    = let (heap', addr) = hAlloc heap (NPrim name prim)
    in (heap', (name, addr))

-- 求值
eval :: TiState -> [TiState]
eval state = state : rest_states 
    where
        rest_states | tiFinal state = []
                    | otherwise = eval next_state
        next_state = doAdmin $ step state

-- do any administrative work between steps
doAdmin :: TiState -> TiState
doAdmin state@(stack, dump, heap, globals, stats) = applyToStats tiStatIncSteps state 

tiFinal :: TiState -> Bool
tiFinal ([result_addr], dump, heap, _, stats) = null dump && isDataNode (hLookup heap result_addr)
tiFinal ([], _, _, _, _) = error "what is fucking going on ? Empty Stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _)= True
isDataNode _ = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch state $ hLookup heap (hd stack) 

dispatch :: TiState -> Node -> TiState
dispatch state (NNum n) = numStep state n
dispatch state (NAp a1 a2) = apStep state a1 a2
dispatch state (NSupercomb sc args body) = scStep state sc args body
dispatch state (NInd a1) = indStep state a1
dispatch state (NPrim name prim) = primStep state prim

numStep :: TiState -> Int -> TiState
numStep ([_], [], heap, globals, stats) _ = error "cannot apply NNum as NAp"
numStep ([_], s:d, heap, globals, stats) _ = (s, d, heap, globals, stats)
numStep state _ =  error $ showResults [state]

apStep :: TiState -> Addr -> Addr -> TiState 
apStep (stack, dump, heap, globals, stats) a1 a2 =
    let a2_node = hLookup heap a2
    in case a2_node of
        NInd a3 -> (stack, dump, hUpdate heap (hd stack) (NAp a1 a3), globals, stats)
        _ -> (a1: stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
    = if length arg_names >= length stack then error $ sc_name ++ " applied to too few values."
        else 
            let
            new_stack = drop (length arg_names) stack
            new_heap = instantiateWithUpdate body (hd $ drop (length arg_names) stack) heap env 
            env = arg_bindings ++ globals
            arg_bindings = zip arg_names (getargs heap stack)
            -- updated_heap = hUpdate new_heap (hd $ drop (length arg_names) stack) $ NInd result_addr
            in (new_stack, dump, new_heap, globals, stats)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack)
    = get_arg <$> stack
    where 
        get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap'' (NAp a1 a2)
    where
        (heap', a1) = instantiate e1 heap env
        (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env = (heap, aLookup env v $ error ("Undefined name " ++ show v))
instantiate (EConstr tag arity) heap env = error ""
instantiate (ELet isrec defs body) heap env = instantiateLet heap env isrec defs body
instantite (ECase e alts) heap env = error "Can't instantiate case expressions"


instantiateWithUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateWithUpdate (ENum n) update_addr heap env = hUpdate heap update_addr (NNum n)
instantiateWithUpdate (EAp e1 e2) update_addr heap env = hUpdate heap'' update_addr (NAp a1 a2)
    where
        (heap', a1) = instantiate e1 heap env
        (heap'', a2) = instantiate e2 heap' env
instantiateWithUpdate (EVar v) update_addr heap env 
    = let addr = aLookup env v $ error ("Undefined name " ++ show v) 
    in hUpdate heap update_addr $ NInd addr

instantiateWithUpdate (EConstr tag arity) _ heap env = error ""
instantiateWithUpdate (ELet isrec defs body) update_addr heap env = 
    let (heap', addr) = instantiateLet heap env isrec defs body
    in hUpdate heap' update_addr $ NInd addr
instantiateWithUpdate (ECase e alts) _ heap env = error "Can't instantiate case expressions"

-- Mark 2
instantiateLet ::TiHeap -> ASSOC Name Addr -> Bool -> [(Name, CoreExpr)] -> CoreExpr -> (TiHeap, Addr)
instantiateLet heap env False defs expr =  -- let
    instantiate expr heap' env'
    where
        instantiateDef env heap (name, expr) = 
            let (heap', addr) = instantiate expr heap env
            in (heap', (name, addr))
        (heap', assoc) = mapAccuml (instantiateDef env) heap defs
        env' = assoc ++ env
instantiateLet heap env True defs expr =   -- letrec
    instantiate expr heap' env'
    where
        instantiateDefRec heap (name, expr) = 
            let (heap', addr) = instantiate expr heap env'
            in (heap', (name, addr))
        (heap', assoc) = mapAccuml instantiateDefRec heap defs
        env' = assoc ++ env

indStep :: TiState -> Addr -> TiState
indStep (stack, dump, heap, globals, stats) a1 = (a1 : tl stack, dump, heap, globals, stats)

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div

primNeg :: TiState -> TiState
primNeg (stack, dump, heap, globals, stats) = 
    (stack', dump', heap', globals, stats)
    where
        arg_addr = hd $ getargs heap stack
        arg = hLookup heap arg_addr
        (stack', dump', heap') = case arg of
            NNum n -> (drop 1 stack, dump, hUpdate heap (hd $ drop 1 stack) $ NNum (-n)) -- rule 2.5
            _ -> ([arg_addr], drop 1 stack : dump, heap)


primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith (stack, dump, heap, globals, stats) arith = 
    (stack', dump', heap', globals, stats)
    where
        arg_addrs = getargs heap stack
        args = hLookup heap <$> arg_addrs
        (stack', dump', heap') = case args of
            [NNum n1, NNum n2] -> (drop 2 stack, dump, hUpdate heap (hd $ drop 2 stack) (NNum (arith n1 n2)))
            [NNum n1, _] -> (tl arg_addrs, drop 2 stack : dump, heap)
            _ -> (take 1 arg_addrs, stack : dump, heap)

-- show
showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [ iLayn $ showState <$> states, showStats (last states)])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) = iConcat [showStack heap stack, iNewline, showHeap heap, iNewline]

showStack :: TiHeap -> TiStack -> Iseq 
showStack heap stack = iConcat [iStr " Stk [", iIndent (iInterleave iNewline (map show_stack_item stack)), iStr " ]"]
                        where 
                        show_stack_item addr = iConcat [showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]
    
showHeap :: TiHeap -> Iseq
showHeap heap = iConcat [
        iStr "Heap [", iIndent (iInterleave iNewline (show_heap_item <$> contentHeap heap)), iStr " ]"]
        where
            show_heap_item (addr, node) = iConcat [ showFWAddr addr, iStr ": ", showNode node]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) = iConcat [ iStr "NAp ", showFWAddr fun_addr, iStr " (", showNode (hLookup heap arg_addr), iStr ")"]
showStkNode heap node = showNode node 

showNode :: Node -> Iseq 
showNode (NAp a1 a2) = iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n
showNode (NInd n) = iStr $ "NInd " ++ show n
showNode (NPrim name primitive) = iStr $ "NPrim " ++ name 

showAddr :: Addr -> Iseq 
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq 
showFWAddr addr = iStr (spaces (4 - length str) ++ str)
                    where 
                        str = show addr 
      
showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) = iConcat [iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats)]


-- data operation functions
initialTiDump :: TiDump
initialTiDump = []

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, sd, stats) = (s, d, h, sd, f stats)