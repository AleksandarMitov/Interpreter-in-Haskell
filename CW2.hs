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
type EnvV = Var -> Aexp

--s_dynamic :: Stm -> State -> State
--s_dynamic (Skip) = state

--Returns a DecP with the updated procedure body
--TODO TEST IT
dyn_update_proc :: DecP -> Pname -> Stm -> DecP
dyn_update_proc procs proc_name proc_body = case elemIndex (proc_name) (fst (unzip procs)) of
                                            Just index -> take index procs ++ [(proc_name, proc_body)] ++ drop (index + 1) procs
                                            Nothing -> procs

--Returns a DecC with the updated var body
--TODO TEST IT
dyn_update_var :: DecV -> Var -> Aexp -> DecV
dyn_update_var vars var_name var_exp = case elemIndex (var_name) (fst (unzip vars)) of
                                            Just index -> take index vars ++ [(var_name, var_exp)] ++ drop (index + 1) vars
                                            Nothing -> vars

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

--Retuns a list of unique var names referenced in the DecV expression
vars_in_decv :: DecV -> [Var]
vars_in_decv ([]) = []
vars_in_decv ((x, y):rest) = nub ([x] ++ vars_in_aexp(y) ++ vars_in_decv(rest))

--Retuns a list of unique var names referenced in the DecP expression
vars_in_decp :: DecP -> [Var]
vars_in_decp ([]) = []
vars_in_decp ((x,y):rest) = nub (vars_in_stm(y) ++ vars_in_decp(rest))

--Retuns a list of unique proc names referenced in the DecP expression
procs_in_decp :: DecP -> [Pname]
procs_in_decp ([]) = []
procs_in_decp ((x,y):rest) = nub ([x] ++ procs_in_stm(y) ++ procs_in_decp(rest))

--START UTILITY STUFF
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
    stmt1 <- try (parens stm) <|> try stm_base
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
