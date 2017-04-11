module ProcParser where
import Control.Applicative
import Prelude hiding (Num)
import Control.Monad (void)
import Data.List (intercalate)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lexer

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Num | V Var | Mult Aexp Aexp
        | Add Aexp Aexp | Sub Aexp Aexp deriving (Show, Eq, Read)
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
        | Le Aexp Aexp | Eq Aexp Aexp deriving (Show, Eq, Read)
data Stm = Skip | Ass Var Aexp | Comp Stm Stm
        | If Bexp Stm Stm | While Bexp Stm
        | Block DecV DecP Stm | Call Pname deriving (Show, Eq, Read)

--START UTILITY STUFF
--List of the Proc language's reserved words
reserved_words :: [String]
reserved_words = ["if","then","else","while","do","skip","true","false","not","call", "proc", "is", "begin", "end", "var"]

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
rword :: String -> Parser ()
rword w = (string w *> notFollowedBy alphaNumChar *> space_consumer)
--END UTILITY STUFF

--Parses a Num
num :: Parser Integer
num = lexeme Lexer.integer

--Parses a Var
var :: Parser String
var = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if elem x reserved_words
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
    <|> try (rword "true"  *> pure (TRUE))
    <|> try (rword "false" *> pure (FALSE))
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
stm_terms = dbg "stm_terms" (parens stm <|> blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> assStm)

--Operators table for Stm expressions
stm_operators :: [[Operator Parser Stm]]
stm_operators =
  [
    [ InfixR (Comp <$ symbol ";") ]
  ]

--Parser for a base Stm
stm_base :: Parser Stm
stm_base = dbg "stm_base" (blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> assStm)

--Parses an if statement
ifStm :: Parser Stm
ifStm = dbg "ifStm" (do
    rword "if"
    cond  <- bexp
    rword "then"
    stmt1 <- stm
    rword "else"
    stmt2 <- stm
    return (If cond stmt1 stmt2))

--Parses a while statement
whileStm :: Parser Stm
whileStm = dbg "whileStm" (do
    rword  "while"
    cond   <- bexp
    rword  "do"
    stmt1  <- stm
    return (While cond stmt1))

--Parses an assign statement
assStm :: Parser Stm
assStm = dbg "assStm" (do
    var  <- var
    symbol ":="
    expr <- aexp
    return (Ass var expr))

--Parses a skip statement
skipStm :: Parser Stm
skipStm = dbg "skipStm" (Skip <$ rword "skip")

--Parses a call statement
callStm :: Parser Stm
callStm = dbg "callStm" (do
    rword "call"
    pname1 <- var
    return (Call pname1))

--Parses a block statement
blockStm :: Parser Stm
blockStm = dbg "blockStm" (do
    rword "begin"
    decv1 <- try decv <|> try (parens decv)
    decp1 <- try decp <|> try (parens decp)
    stm1 <- stm
    rword "end"
    return (Block decv1 decp1 stm1))

--Parses a DecV
decv :: Parser DecV
decv = many (do
    rword "var"
    name  <- var
    symbol ":="
    aexp1 <- aexp
    symbol ";"
    return (name, aexp1))

--Parses a DecP
decp :: Parser DecP
decp = dbg "decp" (many (do
    dbg "decp rword" (rword "proc")
    name <- var
    rword "is"
    stm1 <- dbg "decp stm1" (try stm_base <|> try (parens stm_base) <|> (parens stm))
    symbol ";"
    return (name, stm1)))

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
