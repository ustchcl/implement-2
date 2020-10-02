module Parser where

import Control.Applicative
import Control.Monad
import Language
import Prelude

type Token = (Int, String) -- A token is never empty, Int 是行号

-- 读取文件
-- readFile :: FilePath -> String
-- readFile = error ""
  
-- 词法分析
clex :: Int -> String -> [Token]
clex _ [] = []
clex n (c:cs) | isWhiteSpace c = clex n cs
clex n (c:cs) | not $ isNotEnter c = clex (n+1) cs
clex n (c:cs) | isAlpha c = (n, varTok) : clex n restCs
  where
    varTok = c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs
clex n (c:cs) | isDigit c = (n, numTok) : clex n restCs
  where
    numTok = c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs
clex n (c1:c2:cs) | [c1, c2] == "||" = clex n $ dropWhile (/= '\n') cs -- excersice 1.9 移除注释
clex n (c1:c2:cs) | [c1, c2] `elem` twoCharsOps = (n, [c1, c2]) : clex n cs -- excersice 1.10 双字符操作符

clex n (c:cs) = (n, [c]) : clex n cs

-- 语义分析
syntax :: [Token] -> CoreProgram 
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []): others) = prog
    takeFirstParse (parse : others) = takeFirstParse others
    takeFirstParse other = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: Name -> [Name] -> String -> CoreExpr -> CoreScDefn
mkSc name vars _ expr = (name, vars, expr)

pExpr :: Parser CoreExpr
pExpr = pEVar <~> pENum <~> pEConstr <~> pELet <~> pECase <~> pELam

pEVar :: Parser CoreExpr 
pEVar = pApply pVar EVar

pENum :: Parser CoreExpr 
pENum = pApply pNum ENum

pEConstr :: Parser CoreExpr 
pEConstr = pThen EConstr pNum pNum

mkLetSub :: Name -> String -> CoreExpr -> (Name, CoreExpr)
mkLetSub name _ expr = (name, expr)

pLetSub :: Parser (Name, CoreExpr)
pLetSub = pThen3 mkLetSub pVar (pLit "=") pExpr

mkLet :: Name -> [(Name, CoreExpr)] -> Name -> CoreExpr -> CoreExpr
mkLet keyword list _ expr = ELet (keyword == "letrec") list expr

pELet :: Parser CoreExpr
pELet = pThen4 mkLet (pLit "letrec" <~> pLit "let") (pOneOrMoreWithSep pLetSub $ pLit ";") (pLit "in") pExpr

mkAlter :: [Name] -> Int -> CoreExpr -> CoreAlt
mkAlter vars int expr = (int, vars, expr)

pAlter :: Parser CoreAlt
pAlter = pThen (mkAlter []) pPattern pArrow
  where
    pPattern = pThen3 (\_ x _ -> x) (pLit "<") pNum (pLit ">")
    pArrow = pThen3 (\_ _ x -> x) (pLit "-") (pLit ">") pExpr

pECase :: Parser CoreExpr
pECase = pThen ECase pCaseOf $ pOneOrMoreWithSep pAlter (pLit ";")
  where pCaseOf = pThen3 (\_ x _ -> x) (pLit "case") pExpr (pLit "of")

pELam :: Parser CoreExpr
pELam = pThen ELam pSlashVars pArrow
  where
    pSlashVars = pThen (const id) (pLit "\\") (pOneOrMore pVar)
    pArrow = pThen3 (\_ _ x -> x) (pLit "-") (pLit ">") pExpr





parse :: String -> CoreProgram
parse = syntax . (clex 0)

-- parser 
type Parser a = [Token] -> [(a, [Token])]

data MyParser a = P (Parser a)

instance Functor MyParser where
  fmap f (P g) = P $ \toks -> (\(a, rest) -> (f a, rest)) <$> (g toks)

instance Applicative MyParser where
  pure a = P $ \toks -> [(a, toks)]
  P f <*> pg = P $ \toks -> 
    let parsedF = f toks 
        ff (f', toks') = let P p' = f' <$> pg in p' toks'
    in 
      parsedF >>= ff

instance Monad MyParser where
  return = pure
  P p >>= f = P $ \toks -> 
    let 
      parsedA = p toks 
      ff (a, toks') = let P pb = (f a) in pb toks'
    in parsedA >>= ff


pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat f ((_, tok): toks) = if f tok then [(tok, toks)] else []

pLit :: String -> Parser String
pLit str = pSat (== str)

pVar :: Parser String
pVar = pSat isVariable

pNum :: Parser Int
pNum = pSat isNum `pApply` read

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

(<~>) :: Parser a -> Parser a -> Parser a
(<~>) = pAlt
infixl 4 <~>

-- lift
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen f pa pb = let P pc = liftA2 f (P pa) (P pb) in pc

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 f pa pb pc = let P pd = liftA3 f (P pa) (P pb) (P pc) in pd

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 f pa pb pc pd = let P pe = f <$> (P pa) <*> (P pb) <*> (P pc) <*> (P pd) in pe

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore pa = pOneOrMore pa <~> pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore pa = pThen (:) pa (pZeroOrMore pa)

pApply :: Parser a -> (a -> b) -> Parser b
pApply pa f = let P pb = f <$> P pa in pb

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pa sep = pThen (:) pa (pZeroOrMore $ pThen (const id) sep pa)

-- constants
twoCharsOps :: [String]
twoCharsOps = ["==", "~=", ">=", "<=", "->"]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- utils
isWhiteSpace :: Char -> Bool
isWhiteSpace = flip elem " \t"

isNotEnter :: Char -> Bool 
isNotEnter = (/=) '\n'

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

isDigit :: Char -> Bool
isDigit = (flip elem) ['0'..'9']

isAlpha :: Char -> Bool
isAlpha = (flip elem) (['a'..'z'] ++ ['A'..'Z'])

isVariable :: String -> Bool
isVariable tok@(c:cs) = (not $ tok `elem` keywords) && isAlpha c && all isIdChar cs

isNum :: String -> Bool 
isNum [] = False
isNum (x:_) = isDigit x



