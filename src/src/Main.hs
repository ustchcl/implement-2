module Main where
import Printer
import Language

import Parser

import Template

elet :: CoreProgram
elet = [
    ("elet", [], ELet False letexpr expr)
    ]
    where
        letexpr = [
            ("y", EVar "z"),
            ("z", ENum 1),
            ("ts", EVar "ds")
            ]
        expr = EAp (EVar "f") (EAp (EVar "g") (EVar "x"))

pGreeting :: Parser (String, String)
pGreeting = pThen (,)  pHelloOrGoodbye pVar
    where
        pHelloOrGoodbye = pLit "Hello" <~> pLit "goodbye"


simpleProgram :: [String]
simpleProgram = [
        " g = Pack{1,2};",
        " h = f (f x) ;",
        " f = 3 ;",
        " g x y = let z = x in z ;",
        " h x = case (let y = x in y) of",
        " <1> -> 2 ;",
        " <2> -> 5"
    ]

complexProgram = "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2"

program_1 = "h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5"

program_2 = "main = f 3; f y = S K K y"

programLet = program [
    "main = let y = 3 in S K K y"
    ]

programLetRec = program [
    "pair y z f = f y z;",
    "fst p = p K ;",
    "snd p = p K1 ;",
    "f y z = letrec ",
    "a = pair y b;",
    "b = pair z a",
    "in fst (snd (snd (snd a)));",
    "main = f 3 4"
    ]

programInfinty = program 
    [
        "main = letrec f = f x in f"
    ]

programTwice = program 
    [
        "id x = x;",
        "main = twice twice id 3"
    ]

programNeg = program 
    [
        "y = 3;",
        "x = 6;",
        "main = x * y"
    ]

programFac = program
    [ "fac n = if (n == 0) 1 (n * fac (n-1)) ;"
    , "main = fac 3"
    ]

program :: [String] -> String
program = foldl (\sum str -> sum ++ "\n" ++ str) []

main :: IO ()
main = do 
    putStrLn "Hello ~"
    -- putStrLn $ pprint elet
    -- putStrLn $ show $ splitBy 'a' "abscsdsadsad"
    -- putStrLn $ show $ pGreeting [(1, "goodbye"), (1, "James"), (1, "!")]
    -- -- can you work out why the parsing cost in the previous example rise so fast
    -- putStrLn $ show $ length $ pOneOrMore (pLit "x") $ (\x -> (1, x)) <$> (take 6 (repeat "x"))
    -- putStrLn $ program

    -- putStrLn $ show $ evalMult (2, 3, 0, 0)
    -- putStrLn $ runProg program_let
    putStrLn $ runProg programNeg
    -- putStrLn $ pprint $ (parse programFac)