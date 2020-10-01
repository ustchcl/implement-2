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

  iNil = INil
  iIndent = IIndent
  iStr = IStr
  iAppend = IAppend
  (<+>) = iAppend
  infixl 4 <+>

  iConcat :: [Iseq] -> Iseq
  iConcat = foldl (<+>) iNil

  iNewline :: Iseq
  iNewline = INewline

  instance Show Iseq where
    show = iDisplay

  iDisplay :: Iseq -> String
  iDisplay = error ""


  pprint :: CoreProgram -> String
  pprint = error "todo"

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
  pprExpr _ = error ""

  pprAExpr :: CoreExpr -> Iseq
  pprAExpr e = error ""

  pprDefns :: [(String, CoreExpr)] -> Iseq 
  pprDefns = error ""

  pprDefn :: (String, CoreExpr) -> Iseq
  pprDefn (name, expr) = iConcat [ iStr $ name ++ " = ", iIndent (pprExpr expr) ]


  -- utils
  mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
  mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
    where
      e2s = e2 : e2s

  iInterleave :: Iseq -> [Iseq] -> Iseq
  -- iInterleave sep list = 