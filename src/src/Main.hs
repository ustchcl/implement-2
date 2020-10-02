module Main where
import Printer
import Language

import Parser

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
        " f = 3 ;",
        " g x y = let z = x in z ;",
        " h x = case (let y = x in y) of",
        " <1> -> 2 ;",
        " <2> -> 5"
    ]

program_1 = "f = 3; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5"

program :: [Char]
program = foldl (\sum str -> sum ++ "\n" ++ str) [] simpleProgram

main :: IO ()
main = do 
    putStrLn $ pprint elet
    putStrLn $ show $ splitBy 'a' "abscsdsadsad"
    putStrLn $ show $ pGreeting [(1, "goodbye"), (1, "James"), (1, "!")]
    -- can you work out why the parsing cost in the previous example rise so fast
    putStrLn $ show $ length $ pOneOrMore (pLit "x") $ (\x -> (1, x)) <$> (take 6 (repeat "x"))
    putStrLn $ program
    putStrLn $ show $ syntax (clex 0 program_1)
