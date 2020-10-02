module Main where
import Printer
import Language

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

main :: IO ()
main = putStrLn $ pprint elet
