data Valtype = Symval | Intval | Exprval

data Value = {
  valtype :: Valtype
  , valsym :: String
  , valint :: Int
  , valexpr :: Expr
  } deriving (Show)

data Expr = {
  car :: Value
  , cdr :: Maybe Expr
  } deriving (Show)

main = do
  
