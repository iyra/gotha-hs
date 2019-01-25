data Valtype = Symval | Intval | Exprval

data Value = ValSym String
           | ValInt Int
           | ValExpr Expr
  deriving Show

data Expr = MkExpr
  {
    car :: Value
  , cdr :: Maybe Expr
  } deriving Show

main = do putStrLn "Benis"
          putStrLn "B e n i s"
