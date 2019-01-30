import Data.Char
import Data.List
data Value = ValSym String
           | ValInt Int
           | ValExpr Expr
           | ValString String
           | EmptyExpr
  deriving Show

data Expr = MkExpr
  {
    car :: Maybe Value
  , cdr :: Maybe Expr
  } deriving Show

-- fst = expr
-- snd = string
{-
(deft music (makeStruct '(id Int) '(artist String) '(album String)))
(music-artist (music 1 "David Bowie" "Diamond Dogs"))
-}
parse_int sym r =
  if length sym == 0 then
    Right r
  else
    if not isDigit (head sym) then
      Left "not a digit, so not an int"
    else
      parse_int (tail sym) (r*10)+((ord (head sym))-48)

read_expr e =
  case (car e) of
    Just v -> case v of
      ValSym sym -> case head sym of
        hs | isDigit hs -> case parse_int sym of
               Right ->
    Nothing -> Right e { car=EmptyExpr }

parse :: String -> Expr -> Int -> Bool -> Either String (Expr, String)
parse str e in_expr in_str =
  if length str > 0 then
    case head str of
      '(' | not in_str -> let next = parse (tail str) (MkExpr { car = Nothing, cdr = Nothing }) (in_expr + 1) False
                          in
                            case next of
                              Right n ->
                                case (parse (snd n) (MkExpr { car = Nothing, cdr = Nothing }) in_expr False) of
                                  Right ht -> Right (MkExpr { car = Just $ ValExpr $ fst n, cdr = (case (car (fst ht)) of
                                                                                                     Just _ -> Just $ (fst ht)
                                                                                                     Nothing -> Nothing)}, snd ht)
                                  Left err -> Left err
                              Left err -> Left err
      ')' | not in_str && in_expr > 0 -> Right (e, tail str)
          | not in_str        -> Left "unexpected )"
      c | not in_str && isSpace c -> case (car e) of
                         Nothing -> if in_expr > 0 then (parse (tail str) (MkExpr { car = Nothing, cdr = Nothing }) in_expr False) else Right (e, tail str)
                         Just u -> (if in_expr > 0 then
                                     let p = parse (tail str) (MkExpr { car = Nothing, cdr = Nothing }) in_expr False
                                     in
                                       case p of
                                         Right q -> Right (e { cdr = (case (car (fst q)) of
                                                                       Just _ -> Just (fst q)
                                                                       Nothing -> Nothing)}, snd q)
                                         Left s -> Left s
                                   else
                                     Right (e, tail str))
      c        -> case car e of
                    Nothing -> case c of
                      '"' -> parse (tail str) (e { car=Just $ ValSym "" }) in_expr True
                      _ -> parse (tail str) (e { car=Just $ ValSym $ [c] }) in_expr in_str
                    Just (ValSym s) -> case c of
                      '"' -> if not in_str then
                                Left "found \" but not in string"
                              else
                                parse (tail str) (MkExpr { car = Just $ ValSym $ s, cdr = Nothing }) in_expr False
                      _ -> parse (tail str) (MkExpr { car = Just $ ValSym $ s ++ [c], cdr = Nothing }) in_expr in_str
  else
    if in_expr > 0 then
      Left "unexpected end of input (in_expr > 0)"
    else if in_str then
      Left "end of input while in string"
    else
      Right (e, tail str)

strExpr :: Expr -> String
strExpr e =
  intercalate "" [case car e of
                    Just (ValSym  s) -> s
                    Just (ValInt  i) -> show i
                    Just (ValExpr g) -> "(" ++ strExpr g ++ ")"
                    Nothing          -> "??",
                  --" ",
           (case cdr e of
               Nothing -> ""--"nil"
               Just cdrexpr -> " "++(strExpr cdrexpr))]

main = do
  case (parse "(strlen \"a (xy) b\")" (MkExpr { car = Nothing, cdr = Nothing }) 0 False) of
    Right expr -> do
      putStrLn (strExpr $ fst expr)
      putStrLn $ "remaining " ++ if length (snd expr) > 0 then snd expr else "none"
    Left err -> putStrLn err
