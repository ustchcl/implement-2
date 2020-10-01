module Language where
    type Name = String
    type IsRec = Bool

    recursive :: IsRec
    recursive = True

    nonRecursive :: IsRec
    nonRecursive = False

    -- case
    type Alter a = (Int, [a], Expr a)
    type CoreAlt = Alter Name

    data Expr a 
        = EVar Name                             -- 变量
        | ENum Int                              -- 数字
        | EConstr Int Int                       -- 构造函数
        | EAp (Expr a)  (Expr a)                -- 函数应用
        | ELet IsRec [(a, Expr a)] (Expr a)     -- let 定义 IsRec 是否能递归定义 [(a, Expr a)] 定义 Expr
        | ECase (Expr a) [Alter a]              -- case 表达式
        | ELam [a] (Expr a)                     -- lambda 表达式


    isAtomicExpr :: Expr a -> Bool
    isAtomicExpr (EVar _) = True
    isAtomicExpr (ENum _) = True
    isAtomicExpr _        = False

    type CoreExpr = Expr Name
    type Program a = [ScDefn a]  -- ScDefn: supercombinator definitions
    type CoreProgram = Program Name
    type ScDefn a = (Name, [a], Expr a)  -- [a]: argument list Expr
    type CoreScDefn = ScDefn Name

        -- prelude
    {-
        I x             = x         --id
        K x y           = x         -- const
        K1 x y          = y         -- const . id
        S f g x         = f x (g x)
        compose f g x   = f (g x)
        twice f         = compose f f
    -}
    preludeDefs :: CoreProgram
    preludeDefs = [
        ("I", ["x"], EVar "x"),
        ("K", ["x", "y"], EVar "x"),
        ("K1", ["x", "y"], EVar "y"),
        ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))  (EAp (EVar "g") (EVar "x"))),
        ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
        ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]

 