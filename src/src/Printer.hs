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
  iIndent :: Iseq -> Iseq
  iIndent = IIndent
  iStr :: String -> Iseq
  iStr = IStr
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

  --        current column, 0 is first
  flatten :: Int -> [(Iseq, Int)] -> String
  flatten _ [] = ""
  flatten col ((INil, indent):seqs) = flatten col seqs
  flatten col ((IStr s, indent):seqs) = spaces indent ++ s ++ flatten col seqs
  flatten col ((IAppend s1 s2, indent):seqs) = flatten col ((s1, indent):(s2, indent):seqs)
  flatten _ ((INewline, indent):seqs) = '\n' : (spaces indent) ++ (flatten indent seqs)
  flatten col ((IIndent s, _):seqs) = flatten col $ (s, col) : seqs
  
  pprint :: CoreProgram -> String
  pprint = iDisplay . pprProgram 

  pprExpr :: CoreExpr -> Iseq
  pprExpr (ENum n) = iStr $ show n
  pprExpr (EVar v) =  iStr v
  pprExpr (EConstr i j) = iStr $ "data" ++ show i ++ " " ++ show j
  pprExpr (EAp e1 e2) = (pprExpr e1) <+> (iStr " ") <+> (pprAExpr e2)
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

  pprAExpr :: CoreExpr -> Iseq
  pprAExpr expr 
    | isAtomicExpr expr = pprExpr expr
    | otherwise = iConcat [ iStr "(", pprExpr expr, iStr ")"]

  pprDefns :: [(String, CoreExpr)] -> Iseq 
  pprDefns dfns = iInterleave sep (pprDefn <$> dfns)
    where
      sep = (iStr ";") <+>  iNewline

  pprDefn :: (String, CoreExpr) -> Iseq
  pprDefn (name, expr) = iConcat [ iStr $ name ++ " = ", iIndent (pprExpr expr) ]

  pprAlter :: Alter Name -> Iseq
  pprAlter (_, _, expr) = pprExpr expr

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
      spaces_ = ' ': spaces_