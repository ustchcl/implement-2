module Printer where 
  import Language
  import Utils

  -- Show CoreProgram
  data Iseq 
    = INil
    | IStr String
    | IAppend Iseq Iseq
    | IIndent Iseq
    | INewline

  iNil :: Iseq
  iNil = INil
  iNum :: Int -> Iseq
  iNum = iStr . show
  iFWNum :: Int -> Int -> Iseq
  iFWNum width n 
    = iStr (spaces (width - length digits) ++ digits)
      where
        digits = show n
  iIndent :: Iseq -> Iseq
  iIndent = IIndent
  iStr :: String -> Iseq
  iStr str = iConcat $ replace str
    where -- excersie 1.7
      notLine = (/=) '\n'
      replace :: String -> [Iseq]
      replace [] = []
      replace str@(x:xs) = case x of
        '\n' -> iNewline : replace xs
        _ -> (IStr (takeWhile notLine str)) : (replace (dropWhile notLine str))
  iNewline :: Iseq
  iNewline = INewline
  iAppend :: Iseq -> Iseq -> Iseq
  iAppend = IAppend
  (<+>) :: Iseq -> Iseq -> Iseq
  (<+>) = iAppend
  infixl 4 <+>

  instance Show Iseq where
    show = iDisplay

  iDisplay :: Iseq -> String
  iDisplay seq = flatten 0 [(seq, 0)]

  iLayn :: [Iseq] -> Iseq
  iLayn seqs = iConcat (layItem <$> zip [1..] seqs)
    where
      layItem (n, seq) = iConcat [
          iFWNum 4 n, iStr ")", iIndent seq, iNewline
        ]

  --        current column, 0 is first
  flatten :: Int -> [(Iseq, Int)] -> String
  flatten _ [] = ""
  flatten col ((INil, indent):seqs) = flatten col seqs
  flatten col ((IStr s, _):seqs) = s ++ flatten (col + length s) seqs
  flatten col ((IAppend s1 s2, indent):seqs) = flatten col ((s1, indent):(s2, indent):seqs)
  flatten _ ((INewline, indent):seqs) = '\n' : (spaces indent) ++ (flatten indent seqs)
  flatten col ((IIndent s, _):seqs) = flatten col $ (s, col) : seqs
  
  pprint :: CoreProgram -> String
  pprint = iDisplay . pprProgram 

  pprExpr :: CoreExpr -> Iseq
  pprExpr (ENum n) = iStr $ show n
  pprExpr (EVar v) =  iStr v
  pprExpr (EConstr i j) = iStr $ "Pack{" ++ show i ++ ", " ++ show j ++ "}"
  pprExpr (EAp (EAp (EVar op) e1) e2) | op `elem` ops = iConcat [
      iStr "(", pprExpr e1, iStr op, pprExpr e2, iStr ")"
    ]
  pprExpr (EAp e1 e2) = (iStr "(") <+> (pprExpr e1) <+> (iStr " ") <+> (pprExpr e2) <+> (iStr ")")

  pprExpr (ELet isrec defns expr) = iConcat [ 
      iStr keyword, iNewline,
      iStr " ", iIndent (pprDefns defns), iNewline,
      iStr "in ", pprExpr expr
    ]
    where 
      keyword | not isrec = "let"
              | isrec = "letrec"
  pprExpr (ECase e as)
    = iConcat [
      iStr "case ", pprExpr e, iStr " of", iNewline,
      iStr " ", iIndent.iConcat $ pprAlter <$> as, iNewline
    ]
  pprExpr (ELam vars expr) = 
    iConcat [
      iStr "\\", iStr $ joinWithSpace vars, iStr " -> ", pprExpr expr
    ]
  -- pprExpr _ = error ""

  pprDefns :: [(String, CoreExpr)] -> Iseq 
  pprDefns dfns = iInterleave sep (pprDefn <$> dfns)
    where
      sep = (iStr ";") <+>  iNewline

  pprDefn :: (String, CoreExpr) -> Iseq
  pprDefn (name, expr) = iConcat [ iStr $ name ++ " = ", iIndent (pprExpr expr) ]

  pprAlter :: Alter Name -> Iseq
  pprAlter (pattern, _, expr) = (iStr $ "<" ++ show pattern ++ "> -> ") <+> pprExpr expr <+> iNewline

  pprProgram :: CoreProgram -> Iseq
  pprProgram prog = iConcat $ pprScDefn <$> prog

  pprScDefn :: CoreScDefn -> Iseq
  pprScDefn (name, vars, expr) = 
    iConcat [
      iStr name, iStr $ joinWithSpace vars, iStr " = ", iNewline,
      iStr " ", iIndent $ pprExpr expr, iNewline
    ]

  -- utils
  iConcat :: [Iseq] -> Iseq
  iConcat = foldl (<+>) iNil
  
  iInterleave :: Iseq -> [Iseq] -> Iseq
  iInterleave _ [] = iNil
  iInterleave sep (x:xs) = iInterleave_ sep xs x
    where
      iInterleave_ _ [] joined = joined
      iInterleave_ sep (y:ys) joined = iInterleave_ sep ys $ joined <+> sep <+> y

  joinWithSpace :: [String] -> String
  joinWithSpace = foldl (\acc v -> acc ++ " " ++ v) ""

  spaces :: Int -> String
  spaces = flip take spaces_
    where
      spaces_ = ' ' : spaces_

  splitBy :: Eq a => a -> [a] -> [[a]]
  splitBy _ [] = []
  splitBy a list = if length taked == 0 then splitedRest else taked : splitedRest
    where
      notA = (/=) a
      taked = takeWhile notA list
      rest = drop 1 $ dropWhile notA list
      splitedRest = splitBy a rest

  ops :: [String]
  ops = [
    "+", "-", "*", "/",
    "~=", "==", ">=", "<=", "->",
    ">", "<",
    "&", "|"
    ]