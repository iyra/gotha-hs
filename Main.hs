import Data.Char
data Value = ValSym String
           | ValInt Int
           | ValExpr Expr
  deriving Show

data Expr = MkExpr
  {
    car :: Value
  , cdr :: Maybe Expr
  } deriving Show

-- fst = expr
-- snd = string
parse :: String -> Expr -> Bool -> Int -> Either String (Expr, String)
parse str e carp in_expr =
  if length str > 0 then
    let c = (head str) in
      if c == '(' then
        if carp then
          Left "unexpected ("
        else
          case (parse (tail str) (MkExpr { car=(ValSym ""), cdr = Nothing }) False, in_expr+1) of
            Left err -> Left "error"
            Right next -> (parse (snd next) (MkExpr { car=ValExpr (fst next), cdr=Nothing}) False in_expr)
      else if c == ')' then
        if in_expr > 0 then
          Right (e, tail str)
        else
          Left "unexpected )"
      else if isSpace c then
        if carp == False then
          parse (tail str) (MkExpr { car = (ValSym [c]), cdr = Nothing }) False in_expr
        else
          if in_expr > 0 then
            case (parse (tail str) (MkExpr { car=(ValSym ""), cdr = Nothing }) False in_expr) of
              Left err -> Left err
              Right p -> Right (e { cdr = Just (fst p) }, (snd p)) 
          else
            Right (e, tail str)
      else if isDigit c then
        if carp == False then
          parse (tail str) (MkExpr { car = (ValSym [c]), cdr = Nothing }) True in_expr
        else
          case (car e) of
            ValSym s -> parse (tail str) (MkExpr { car = (ValSym (s ++ [c])), cdr = Nothing }) True in_expr
            _ -> Left "unexpected symbol type in parse"
      else
        Left "unexpected type of character"
  else
    if carp then
      Right (e, tail str)
    else
      Left "empty input"

strExpr :: Expr -> String
strExpr e =
  unwords [(case (car e) of
              ValSym s -> s
              ValInt i -> show i
              ValExpr g -> strExpr g),
           (case (cdr e) of
               Nothing -> ""
               Just cdrexpr -> strExpr cdrexpr)]

main = do
  case (parse "123" (MkExpr { car=(ValSym ""), cdr = Nothing }) False) of
    Right expr -> putStrLn (strExpr (fst expr))
    Left err -> putStrLn err
