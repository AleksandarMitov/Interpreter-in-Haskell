module ProcParser where
import Control.Applicative
import Prelude hiding (Num)
import Control.Monad (void)
import Data.List (intercalate, nub, elemIndex)
import Text.Megaparsec hiding (parse, State)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lexer

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type T = Bool
type Z = Integer
type State = Var -> Z

data Aexp = N Num | V Var | Mult Aexp Aexp
        | Add Aexp Aexp | Sub Aexp Aexp deriving (Show, Eq, Read)
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
        | Le Aexp Aexp | Eq Aexp Aexp deriving (Show, Eq, Read)
data Stm = Skip | Ass Var Aexp | Comp Stm Stm
        | If Bexp Stm Stm | While Bexp Stm
        | Block DecV DecP Stm | Call Pname deriving (Show, Eq, Read)

type EnvP = Pname -> Stm

--data type with a constructor taking a proc name, its associated stm body
--and a list of available procs to be called from the current proc
data MixedProc = MixedProc Pname Stm [MixedProc] DecP

--TODO TEST IT
s_dynamic :: Stm -> State -> State
s_dynamic expr state = dyn_get_var (fst (stm_val vars procs expr))
                        where var_names = vars_in_stm expr
                              proc_names = procs_in_stm expr
                              vars = extract_state state var_names
                              procs = zip proc_names (repeat Skip)

--Returns a list of var names paired with their values extracted from the state function
extract_state :: State -> [Var] -> [(Var, Z)]
extract_state state [] = []
extract_state state (var:vars) = (var, (state var)) : (extract_state state vars)

--Evaluates an Stm expression with dynamic vars and procs
--Implementing the table with rules on p.54
--TODO TEST IT
stm_val :: [(Var, Z)] -> [(Pname, Stm)] -> Stm -> ([(Var, Z)],  [(Pname, Stm)])
stm_val vars procs (Skip) = (vars, procs)
stm_val vars procs (Ass var expr) = (dyn_update_var vars var (aexp_val (dyn_get_var vars) expr), procs)
stm_val vars procs (Comp stm1 stm2) = stm_val v1 p1 stm2
                                    where s1 = stm_val vars procs stm1
                                          v1 = fst s1
                                          p1 = snd s1
stm_val vars procs (If bexpr stm1 stm2) = case (bexp_val (dyn_get_var vars) bexpr) of
                                    True -> stm_val vars procs stm1
                                    False -> stm_val vars procs stm2
stm_val vars procs (While bexpr stm) = case (bexp_val (dyn_get_var vars) bexpr) of
                                    True -> stm_val v1 p1 (While bexpr stm)
                                    False -> (vars, procs)
                                    where s1 = stm_val vars procs stm
                                          v1 = fst s1
                                          p1 = snd s1
stm_val vars procs (Block decv decp stm) = (result_state, procs)
                                    where v1 = decv_val vars decv
                                          p1 = decp_val procs decp
                                          s1 = stm_val v1 p1 stm
                                          updated_state = fst s1
                                          local_vars = local_vars_in_decv decv
                                          result_state = map (\(x, y) -> if elem x local_vars
                                              then (x, dyn_get_var vars x)
                                              else (x, y)) updated_state
stm_val vars procs (Call pname) = stm_val vars procs (dyn_get_proc procs pname)

--Evaluates an Stm expression with dynamic vars and static procs
--TODO TEST IT
stm_val_mixed :: [(Var, Z)] -> MixedProc -> [(Var, Z)]
stm_val_mixed vars (MixedProc pname Skip procs decp) = vars
stm_val_mixed vars (MixedProc pname (Ass var expr) procs decp) = dyn_update_var vars var (aexp_val (dyn_get_var vars) expr)
stm_val_mixed vars (MixedProc pname (Comp stm1 stm2) procs decp) = stm_val_mixed updated_vars (MixedProc pname stm2 procs decp)
                            where updated_vars = stm_val_mixed vars (MixedProc pname stm1 procs decp)
stm_val_mixed vars (MixedProc pname (If bexpr stm1 stm2) procs decp) = case (bexp_val (dyn_get_var vars) bexpr) of
                            True -> stm_val_mixed vars (MixedProc pname stm1 procs decp)
                            False -> stm_val_mixed vars (MixedProc pname stm2 procs decp)
stm_val_mixed vars (MixedProc pname (While bexpr stm) procs decp) = case (bexp_val (dyn_get_var vars) bexpr) of
                            True -> stm_val_mixed updated_vars (MixedProc pname (While bexpr stm) procs decp)
                            False -> vars
                            where updated_vars = stm_val_mixed vars (MixedProc pname stm procs decp)
stm_val_mixed vars (MixedProc pname (Block decv decp stm) procs decp0) = result_state
                            where v1 = decv_val vars decv
                                  p1 = static_decp_val procs decp decp
                                  updated_state = stm_val_mixed v1 (MixedProc pname stm p1 decp0)
                                  local_vars = local_vars_in_decv decv
                                  result_state = map (\(x, y) -> if elem x local_vars
                                      then (x, dyn_get_var vars x)
                                      else (x, y)) updated_state
stm_val_mixed vars (MixedProc pname (Call call_proc) procs decp) = case elemIndex call_proc (fst (unzip decp)) of
                            Just index -> stm_val_mixed vars (MixedProc call_proc (snd (decp !! index)) procs decp)
                            Nothing -> stm_val_mixed vars (MixedProc call_proc stm_proc subproc_procs subproc_decp)
                            where stm_proc = case (static_get_proc procs call_proc) of
                                        MixedProc pn sb ps decp1 -> sb
                                  subproc_procs = case (static_get_proc procs call_proc) of
                                        MixedProc pn sb ps decp1 -> ps
                                  subproc_decp = case (static_get_proc procs call_proc) of
                                        MixedProc pn sb ps decp1 -> decp1

--Evaluates an Aexp expression
aexp_val :: State -> Aexp -> Z
aexp_val state (N num) = num
aexp_val state (V var) = state var
aexp_val state (Mult aexp1 aexp2) = (aexp_val state aexp1) * (aexp_val state aexp2)
aexp_val state (Add aexp1 aexp2) = (aexp_val state aexp1) + (aexp_val state aexp2)
aexp_val state (Sub aexp1 aexp2) = (aexp_val state aexp1) - (aexp_val state aexp2)

--Evaluates a Bexp expression
bexp_val :: State -> Bexp -> T
bexp_val state (TRUE) = True
bexp_val state (FALSE) = False
bexp_val state (Neg expr) = not (bexp_val state expr)
bexp_val state (And expr1 expr2) = (bexp_val state expr1) && (bexp_val state expr2)
bexp_val state (Le expr1 expr2) = (aexp_val state expr1) <= (aexp_val state expr2)
bexp_val state (Eq expr1 expr2) = (aexp_val state expr1) == (aexp_val state expr2)

--Evaluates a DecV expression
decv_val :: [(Var, Z)] -> DecV -> [(Var, Z)]
decv_val vars [] = vars
decv_val vars ((var_name, var_exp):rest) = decv_val (dyn_update_var vars var_name (aexp_val (dyn_get_var vars) var_exp)) rest

--Evaluates a DecP expression
decp_val :: [(Pname, Stm)] -> DecP -> [(Pname, Stm)]
decp_val procs [] = procs
decp_val procs ((proc_name, proc_expr):rest) = decp_val (dyn_update_proc procs proc_name proc_expr) rest

--Evaluates a DecP expression
--injecting the current proc body twice via the injected_current_proc variable in order to support recursion
--with this, the current proc will have its body available in the list of procs that the proc has access to
static_decp_val :: [MixedProc] -> DecP -> DecP -> [MixedProc]
static_decp_val procs [] same_level_decps = procs
static_decp_val procs ((proc_name, proc_expr):rest) same_level_decps = static_decp_val updated_list rest same_level_decps
                                where injected_current_proc = static_update_proc procs proc_name proc_expr procs same_level_decps
                                      updated_list = static_update_proc procs proc_name proc_expr injected_current_proc same_level_decps

--Returns a DecP with the updated procedure body
dyn_update_proc :: [(Pname, Stm)] -> Pname -> Stm -> [(Pname, Stm)]
dyn_update_proc procs proc_name proc_body = case elemIndex (proc_name) (fst (unzip procs)) of
                                            Just index -> take index procs ++ [(proc_name, proc_body)] ++ drop (index + 1) procs
                                            Nothing -> procs

--Returns a DecP with the updated procedure body
static_update_proc :: [MixedProc] -> Pname -> Stm -> [MixedProc] -> DecP -> [MixedProc]
static_update_proc [] proc_name proc_body proc_procs proc_decp = [MixedProc proc_name proc_body proc_procs proc_decp]
static_update_proc ((MixedProc pname pbody pprocs pdecp):rest) proc_name proc_body proc_procs proc_decp = if proc_name == pname
    then (MixedProc pname proc_body proc_procs proc_decp) : rest
    else (MixedProc pname pbody pprocs pdecp) : (static_update_proc rest proc_name proc_body proc_procs proc_decp)

--Returns a list of tuples with the updated var value
dyn_update_var :: [(Var, Z)] -> Var -> Z -> [(Var, Z)]
dyn_update_var vars var_name var_val = case elemIndex (var_name) (fst (unzip vars)) of
                                            Just index -> take index vars ++ [(var_name, var_val)] ++ drop (index + 1) vars
                                            Nothing -> vars

--Returns the Stm associated with the proc
dyn_get_proc :: [(Pname, Stm)] -> EnvP
dyn_get_proc procs proc_name = case elemIndex (proc_name) (fst (unzip procs)) of
                                Just index -> snd (procs !! index)
                                Nothing -> Skip

--Returns the MixedProc associated with the proc name
static_get_proc :: [MixedProc] -> Pname -> MixedProc
static_get_proc [] proc_name = MixedProc proc_name Skip [] []
static_get_proc ((MixedProc pname pbody pprocs pdecp):rest) proc_name = if proc_name == pname
    then MixedProc pname pbody pprocs pdecp
    else static_get_proc rest proc_name

--Returns the Z val associated with the var
dyn_get_var :: [(Var, Z)] -> State
dyn_get_var vars var_name = case elemIndex (var_name) (fst (unzip vars)) of
                                Just index -> snd (vars !! index)
                                Nothing -> 0

--Retuns a list of unique var names referenced in the Stm expression
vars_in_stm :: Stm -> [Var]
vars_in_stm (Skip) = []
vars_in_stm (Ass var aexp) = nub (vars_in_aexp(aexp) ++ [var])
vars_in_stm (Comp stm1 stm2) = nub (vars_in_stm(stm1) ++ vars_in_stm(stm2))
vars_in_stm (If bexp stm1 stm2) = nub (vars_in_bexp(bexp) ++ vars_in_stm(stm1) ++ vars_in_stm(stm2))
vars_in_stm (While bexp stm) = nub (vars_in_bexp(bexp) ++ vars_in_stm(stm))
vars_in_stm (Block decv decp stm) = nub (vars_in_decv(decv) ++ vars_in_decp(decp) ++ vars_in_stm(stm))
vars_in_stm (Call pname) = []

--Retuns a list of unique proc names referenced in the Stm expression
procs_in_stm :: Stm -> [Pname]
procs_in_stm (Skip) = []
procs_in_stm (Ass var aexp) = []
procs_in_stm (Comp stm1 stm2) = nub (procs_in_stm(stm1) ++ procs_in_stm(stm2))
procs_in_stm (If bexp stm1 stm2) = nub (procs_in_stm(stm1) ++ procs_in_stm(stm2))
procs_in_stm (While bexp stm) = procs_in_stm(stm)
procs_in_stm (Block decv decp stm) = nub (procs_in_decp(decp) ++ procs_in_stm(stm))
procs_in_stm (Call pname) = [pname]

--Retuns a list of unique var names referenced in the Aexp expression
vars_in_aexp :: Aexp -> [Var]
vars_in_aexp (N num) = []
vars_in_aexp (V var) = [var]
vars_in_aexp (Mult aexp1 aexp2) = nub (vars_in_aexp(aexp1) ++ vars_in_aexp(aexp2))
vars_in_aexp (Add aexp1 aexp2) = nub (vars_in_aexp(aexp1) ++ vars_in_aexp(aexp2))
vars_in_aexp (Sub aexp1 aexp2) = nub (vars_in_aexp(aexp1) ++ vars_in_aexp(aexp2))

--Retuns a list of unique var names referenced in the Bexp expression
vars_in_bexp :: Bexp -> [Var]
vars_in_bexp (TRUE) = []
vars_in_bexp (FALSE) = []
vars_in_bexp (Neg bexp) = vars_in_bexp(bexp)
vars_in_bexp (And bexp1 bexp2) = nub (vars_in_bexp(bexp1) ++ vars_in_bexp(bexp2))
vars_in_bexp (Le aexp1 aexp2) = nub (vars_in_aexp(aexp1) ++ vars_in_aexp(aexp2))
vars_in_bexp (Eq aexp1 aexp2) = nub (vars_in_aexp(aexp1) ++ vars_in_aexp(aexp2))

--Retuns a list of unique var names ***referenced*** in the DecV expression
vars_in_decv :: DecV -> [Var]
vars_in_decv ([]) = []
vars_in_decv ((x, y):rest) = nub ([x] ++ vars_in_aexp(y) ++ vars_in_decv(rest))

--Retuns a list of unique var names ***declared*** in the DecV expression
--Implementation of DV(Dv) in the book, p.51
local_vars_in_decv :: DecV -> [Var]
local_vars_in_decv ([]) = []
local_vars_in_decv ((x, y):rest) = nub ([x] ++ local_vars_in_decv(rest))

--Retuns a list of unique var names referenced in the DecP expression
vars_in_decp :: DecP -> [Var]
vars_in_decp ([]) = []
vars_in_decp ((x,y):rest) = nub (vars_in_stm(y) ++ vars_in_decp(rest))

--Retuns a list of unique proc names referenced in the DecP expression
procs_in_decp :: DecP -> [Pname]
procs_in_decp ([]) = []
procs_in_decp ((x,y):rest) = nub ([x] ++ procs_in_stm(y) ++ procs_in_decp(rest))

--START PARSER RELATED API
--List of the Proc language's reserved words
list_of_reserved_words :: [String]
list_of_reserved_words = ["if","then","else","while","do","skip","true","false","not","call", "proc", "is", "begin", "end", "var"]

--Handling whitespace and comments
space_consumer :: Parser ()
space_consumer = (Lexer.space (void spaceChar) lineCmnt blockCmnt)
    where lineCmnt  = Lexer.skipLineComment "//"
          blockCmnt = Lexer.skipBlockComment "/*" "*/"

--Wrapper for the space consumer
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space_consumer

--Parses a string and any whitespace after it
symbol :: String -> Parser String
symbol = Lexer.symbol space_consumer

--Parses something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--Parses a reserved word
reserved_word :: String -> Parser ()
reserved_word w = (string w *> notFollowedBy alphaNumChar *> space_consumer)
--END UTILITY STUFF

--Parses a Num
num :: Parser Integer
num = lexeme Lexer.integer

--Parses a Var
var :: Parser String
var = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
        check x = if elem x list_of_reserved_words
                    then fail $ "string " ++ show x ++ " can't be a var name"
                  else return x

--Parses an Aexp
aexp :: Parser Aexp
aexp = makeExprParser aexp_terms aexp_operators

--Terms for the Aexp expression parser aexp
aexp_terms :: Parser Aexp
aexp_terms = parens aexp <|> V <$> var <|> N <$> num

--Operators table for Aexp expressions
aexp_operators :: [[Operator Parser Aexp]]
aexp_operators =
  [
    [ InfixL (Mult <$ symbol "*") ],
    [ InfixL (Add  <$ symbol "+"), InfixL (Sub <$ symbol "-") ]
  ]

--Parses a Bexp
bexp :: Parser Bexp
bexp = makeExprParser bexp_terms bexp_operators

--Terms for the Bexp expression parser bexp
bexp_terms :: Parser Bexp
bexp_terms =  parens bexp
    <|> try (reserved_word "true"  *> pure (TRUE))
    <|> try (reserved_word "false" *> pure (FALSE))
    <|> try le
    <|> try eq

--Operators table for Bexp expressions
bexp_operators :: [[Operator Parser Bexp]]
bexp_operators =
  [
    [ Prefix (Neg <$ symbol "!") ],
    [ InfixL (And <$ symbol "&") ]
  ]

--Parses an Le boolean expression
le :: Parser Bexp
le = do
  a1 <- aexp
  symbol "<="
  a2 <- aexp
  return (Le a1 a2)

--Parses an Eq boolean expression
eq :: Parser Bexp
eq = (do
  a1 <- aexp
  symbol "="
  a2 <- aexp
  return (Eq a1 a2))

--Parses an Stm
stm :: Parser Stm
stm = makeExprParser stm_terms stm_operators

--Terms for the Stm expression parser stm
stm_terms :: Parser Stm
stm_terms = parens stm <|> blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> assStm

--Operators table for Stm expressions
stm_operators :: [[Operator Parser Stm]]
stm_operators =
  [
    [ InfixR (Comp <$ symbol ";") ]
  ]

--Parser for a base Stm
stm_base :: Parser Stm
stm_base = blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> assStm

--Parses an if statement
ifStm :: Parser Stm
ifStm = do
    reserved_word "if"
    cond  <- bexp
    reserved_word "then"
    stmt1 <- stm
    reserved_word "else"
    stmt2 <- try (parens stm) <|> try stm_base
    return (If cond stmt1 stmt2)

--Parses a while statement
whileStm :: Parser Stm
whileStm = do
    reserved_word  "while"
    cond   <- bexp
    reserved_word  "do"
    stmt1  <- try (parens stm) <|> try stm_base
    return (While cond stmt1)

--Parses an assign statement
assStm :: Parser Stm
assStm = do
    var  <- var
    symbol ":="
    expr <- aexp
    return (Ass var expr)

--Parses a skip statement
skipStm :: Parser Stm
skipStm = Skip <$ reserved_word "skip"

--Parses a call statement
callStm :: Parser Stm
callStm = do
    reserved_word "call"
    pname1 <- var
    return (Call pname1)

--Parses a block statement
blockStm :: Parser Stm
blockStm = do
    reserved_word "begin"
    decv1 <- try decv <|> try (parens decv)
    decp1 <- try decp <|> try (parens decp)
    stm1 <- stm
    reserved_word "end"
    return (Block decv1 decp1 stm1)

--Parses a DecV
decv :: Parser DecV
decv = many (do
    reserved_word "var"
    name  <- var
    symbol ":="
    aexp1 <- aexp
    symbol ";"
    return (name, aexp1))

--Parses a DecP
decp :: Parser DecP
decp = many (do
    reserved_word "proc"
    name <- var
    reserved_word "is"
    stm1 <- try stm_base <|> (parens stm)
    symbol ";"
    return (name, stm1))

--Parses the string and returns the resulting AST
--Returns Skip on failure
parse :: String -> Stm
parse str = case (parseMaybe (between space_consumer eof stm) str) of
    Just result -> result
    Nothing -> Skip

--Parses a given file and returns the AST representation as a string
parseFile :: FilePath -> IO ()
parseFile filePath = do
  file <- readFile filePath
  putStrLn $ case parseMaybe (between space_consumer eof stm) file of
    Nothing   -> show "Error while parsing"
    Just prog -> show prog

main = putStrLn "Welcome to the parser implementation for the Proc language!"

testDyn :: FilePath -> IO ()
testDyn filePath = do
  file <- readFile filePath
  putStrLn $ case parseMaybe (between space_consumer eof stm) file of
    Nothing   -> show "Error while parsing"
    Just prog -> show (stm_val vars procs prog)
                    where vars = zip (vars_in_stm prog) (repeat 0)
                          procs = zip (procs_in_stm prog) (repeat Skip)

testMixed :: FilePath -> IO ()
testMixed filePath = do
    file <- readFile filePath
    putStrLn $ case parseMaybe (between space_consumer eof stm) file of
        Nothing   -> show "Error while parsing"
        Just prog -> show (stm_val_mixed vars (MixedProc "" prog [] []))
                     where vars = zip (vars_in_stm prog) (repeat 0)
                           procs = map (\x -> MixedProc x Skip [] []) (procs_in_stm prog)

testVars :: FilePath -> IO ()
testVars filePath = do
  file <- readFile filePath
  putStrLn $ case parseMaybe (between space_consumer eof stm) file of
    Nothing   -> show "Error while parsing"
    Just prog -> show (intercalate ", " (vars_in_stm(prog)))

testProcs :: FilePath -> IO ()
testProcs filePath = do
  file <- readFile filePath
  putStrLn $ case parseMaybe (between space_consumer eof stm) file of
    Nothing   -> show "Error while parsing"
    Just prog -> show (intercalate ", " (procs_in_stm(prog)))
