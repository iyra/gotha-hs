import Data.Char
import Data.List
import Data.Map (Map, fromList, toList)            -- This just imports the type name
data Ty = str | int | expr
data Value = ValSym String
           | ValInt Int
           | ValExpr Expr
           | ValString String
           | ValBuiltinFn String
           | ValFn [Expr] Value -- Function: [(argname . argtype)] argmethod
           | ValCons Value Value
           | EmptyExpr
  deriving Show
data 
data Expr = MkExpr
  {
    car :: Maybe Value
  , cdr :: Maybe Expr
  } deriving Show

-- fst = expr
-- snd = string
{-
(define myType (list int int int))
(define myBool (? 'true 'false))
(define myMaybeInt (? int 'Nothing))
(define myBoolVal 'true)
(define questionAnswer (lambda ((x (? myBool 'Nothing))) (cond x (('Nothing "oops") ('true "yay") ('false "nay")))))
(Deft music (makeStruct '(id Int) '(artist String) '(album String)))
(music-artist (music 1 "David Bowie" "Diamond Dogs"))
-}

{-eval_expr e =
  case (car e) of
    Just v -> case v of
      Val-}

size_expr :: Maybe Expr -> Int -> Int
size_expr e i =
  case e of
    Nothing -> i
    Just u -> case (cdr u) of
                Just h -> size_expr (Just h) (i+1)
                Nothing -> i+1

--check_func expected_size expected_args =
  

eval_expr :: Expr -> Map String Value -> Either String Value
eval_expr e b =
  case (car e) of
    Just (ValExpr expr) ->
      let fn = (car expr)
          args = (cdr expr) in
        case fn of
          Nothing -> Left "function call with empty function specifier"
          Just fnv -> let evald_fnv = (eval_expr MkExpr { car=Just fnv, cdr=Nothing } b) in
            case evald_fnv of
              Left err -> Left err
              Right fn_val -> case fn_val of
                                 ValBuiltinFn builtin -> case builtin of
                                                            "quote" -> (case size_expr args 0 of
                                                                          0 -> Left ("quote is unary, nullary given" ++ (case args of
                                                                                                                           Just q -> (strExpr q)
                                                                                                                           Nothing -> ""))
                                                                          1 -> (case args of
                                                                                  Just av -> (case (car av) of
                                                                                                Just h -> Right h
                                                                                                Nothing -> Left "??")
                                                                                  Nothing -> Left "??")
                                                                          n -> Left ("quote is unary, "++show(n)++"-ary given"))
                                                            _ -> Left "unrecognised builtin"
                                 _ -> Left "unrecognised type of function"
    Just (ValSym s) -> case s of
      "quote" -> Right (ValBuiltinFn "quote")
      _ -> case lookup s (toList b) of
             Nothing -> Left ("unbound identifier: " ++ s)
             Just found_bind -> Right found_bind
    Just care -> Right care
    Nothing -> Left "expected a car"

is_cons :: Expr -> Maybe ValCons
is_cons e =
  case (car e) of
    Nothing -> False
    Just _ -> case (cdr e) of
      Nothing -> False
      Just d -> case (car d) of
        Nothing -> False
        Just (ValSym s) -> if s == "." then
                             case (cdr d) of
                               Nothing -> False
                               Just q -> case (cdr q) of
                                 Nothing -> True
                                 _ -> False
                           else
                             False

parse_int :: String -> Int -> Either String Int
parse_int sym r =
  if length sym == 0 then
    Right r
  else
    if not (isDigit (head sym)) then
      Left "not a digit, so not an int"
    else
      parse_int (tail sym) ((r*10)+((ord (head sym))-48))

read_expr e =
  case (car e) of
    Just v -> case v of
      ValSym sym -> case head sym of
        c | isInfixOf " " sym -> case (cdr e) of
                                   Just cdrv -> (let h = read_expr cdrv in
                                                   case h of
                                                     Right x -> Right (MkExpr { car=Just(ValString sym), cdr=Just x })
                                                     Left err -> Left err)
                                   Nothing -> Right e { car = Just(ValString sym) }
        hs | isDigit hs -> case (parse_int sym 0) of
                             Right iv -> case (cdr e) of
                                           Just cdrv -> (let h = read_expr cdrv in
                                                           case h of
                                                             Right x -> Right (MkExpr { car=Just(ValInt iv), cdr=Just x })
                                                             Left err -> Left err)
                                           Nothing -> Right e { car = Just (ValInt iv) }
                             Left err -> Left err
       
        _ -> case (cdr e) of
                  Just cdrv -> (let h = read_expr cdrv in
                                  case h of
                                    Right x -> Right (e { cdr=Just x })
                                    Left err -> Left err)
                  Nothing -> Right e
      ValExpr expr -> let expr_read = read_expr expr in
                        case expr_read of
                          Right x -> case (cdr e) of
                                       Just cdrv -> (let h = read_expr cdrv in
                                                        case h of
                                                          Right cdrx -> Right (MkExpr { car=Just(ValExpr x), cdr=Just cdrx })
                                                          Left err -> Left err)
                                       Nothing -> Right e { car=Just(ValExpr x) }
                          Left err -> Left err
      _ -> Right e
    Nothing -> Right e { car=Just EmptyExpr }

parse :: String -> Expr -> Int -> Bool -> Either String (Expr, String)
parse str e in_expr in_str =
  if length str > 0 then
    case head str of
      '\'' | not in_str -> let quoted = parse (tail str) (MkExpr { car = Nothing, cdr = Nothing }) (in_expr) False in
                             case quoted of
                               Right q -> Right ((MkExpr { car = Just $ ValExpr (MkExpr { car = Just $ ValSym $ "quote",
                                                                                          cdr = Just $ (MkExpr {car=(car (fst q)),cdr=Nothing})}),
                                                           cdr = (cdr (fst q))}), snd q)
                               Left err -> Left err
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

strVal :: Maybe Value -> String
strVal v =
  case v of
    Just (ValSym  s) -> "[sym "++s++"]"
    Just (ValInt  i) -> "[int "++show i++"]"
    Just (ValExpr g) -> "(" ++ strExpr g ++ ")"
    Just (ValString s) -> "\""++s++"\""
    Just EmptyExpr -> "()"
    Nothing          -> "??"

strExpr :: Expr -> String
strExpr e =
  intercalate "" [strVal (car e),
                  --" ",
           (case cdr e of
               Nothing -> ""--"nil"
               Just cdrexpr -> " "++(strExpr cdrexpr))]

main = do
  case (parse "((a b) . 2)" (MkExpr { car = Nothing, cdr = Nothing }) 0 False) of
    Right expr -> do
      putStrLn (strExpr $ fst expr)
      case read_expr (fst expr) of
        Left err -> putStrLn err
        Right ce -> putStrLn ("evaluated: " ++ case (eval_expr ce (fromList [])) of
                                                Left err -> err
                                                Right v -> strVal (Just v))
      putStrLn $ "remaining " ++ if length (snd expr) > 0 then snd expr else "none"
    Left err -> putStrLn err
