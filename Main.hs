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

parse str e carp =
  if length str > 0 then
    let c = (head str) in
      if isDigit c then
        if carp == False then
          parse (tail str) (MkExpr { car = (ValSym [c]), cdr = Nothing }) True
        else
          case (car e) of
            ValSym s -> parse (tail str) (MkExpr { car = (ValSym (s + [c])), cdr = Nothing }) True
            _ -> Left "unexpected symbol type in parse"
      else
        Left "unexpected type of character"
  else
    if carp then
      Right e
    else
      Left "empty input"

strExpr e =
    [(case (car e) of
      ValSym s -> s
      ValInt i -> show i
      ValExpr g -> strExpr g),
    (case (cdr e) of
      Nothing -> ""
      Just cdrexpr -> strExpr cdrexpr)]

main = do
  case (parse "1" (MkExpr { car=(ValSym ""), cdr = Nothing }) False) of
    Right expr -> (let estrs = (strExpr expr) in
      do
        putStrLn (head estrs)
        putStrLn (tail estrs))
    Left err -> putStrLn err
