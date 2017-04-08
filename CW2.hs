module Main where
import Control.Applicative
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Monad (void)
import Data.List (intercalate)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Num | V Var | Mult Aexp Aexp
        | Add Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
        | Le Aexp Aexp | Eq Aexp Aexp
data Stm = Skip | Ass Var Aexp | Comp Stm Stm
        | If Bexp Stm Stm | While Bexp Stm
        | Block DecV DecP Stm | Call Pname

--START UTILITY STUFF
rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","&&","||", "call", "proc", "is", ":=", "<=", "="]

-- handling whitespace and comments
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

-- wraper for the space consumer
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--Since we often want to parse some “fixed” string, let’s define one more parser called symbol.
--It will take a string as argument and parse this string and whitespace after it.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

word :: String -> Parser ()
word w = string w *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
--END UTILITY STUFF



whileParser :: Parser Stm
whileParser = between sc eof stm

stm :: Parser Stm
stm = parens (ifStm <|> whileStm <|> skipStm <|> assStm <|> compStm <|> blockStm <|> callStm)

-- TODO
decv :: Parser DecV
decv = sepBy varpair (symbol ";")

-- TODO
varpair :: Parser (Var,Aexp)
varpair = do
    rword "var"
    name <- identifier
    rword ":="
    aexp1 <- aexp
    return (name, aexp1)

-- TODO
decp :: Parser DecP
decp = sepBy callpair (symbol ";")

-- TODO
callpair :: Parser (Pname,Stm)
callpair = do
    rword "proc"
    name <- pname
    rword "is"
    stm1 <- stm
    return (name, stm1)

-- TODO
pname :: Parser Pname
pname = some alphaNumChar

-- TODO
var :: Parser Var
var = identifier

-- TODO
num :: Parser Num
num = integer

aexp :: Parser Aexp
aexp = makeExprParser aTerm aOperators

bexp :: Parser Bexp
bexp = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser Aexp]]
aOperators =
  [
    [ InfixL (Mult <$ symbol "*")]
  , [ InfixL (Add  <$ symbol "+")
    , InfixL (Sub  <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser Bexp]]
bOperators =
  [ [Prefix (Neg <$ rword "!") ]
  , [InfixL (And <$ rword "&&")]
  ]

aTerm :: Parser Aexp
aTerm = parens aexp
  <|> V      <$> var
  <|> N      <$> num

bTerm :: Parser Bexp
bTerm =  parens bexp
    <|> (rword "true"  *> pure (TRUE))
    <|> (rword "false" *> pure (FALSE))
    <|> le
    <|> eq

le :: Parser Bexp
le = do
    a1 <- aexp
    op <- rword "<="
    a2 <- aexp
    return (Le a1 a2)

eq :: Parser Bexp
eq = do
    a1 <- aexp
    op <- symbol "="
    a2 <- aexp
    return (Eq a1 a2)


ifStm :: Parser Stm
ifStm = do
    rword "if"
    cond  <- bexp
    rword "then"
    stmt1 <- stm
    rword "else"
    stmt2 <- stm
    return (If cond stmt1 stmt2)

whileStm :: Parser Stm
whileStm = do
    rword "while"
    cond <- bexp
    rword "do"
    stmt1 <- stm
    return (While cond stmt1)

assStm :: Parser Stm
assStm = do
    var  <- identifier
    void (symbol ":=")
    expr <- aexp
    return (Ass var expr)

skipStm :: Parser Stm
skipStm = Skip <$ rword "skip"

-- TODO
compStm :: Parser Stm
compStm = do
    stm1  <- stm
    void (symbol ";")
    stm2 <- stm
    return (Comp stm1 stm2)

-- TODO
blockStm :: Parser Stm
blockStm = do
    rword "begin"
    decv1 <- decv
    decp1 <- decp
    stm1 <- stm
    rword "end"
    return (Block decv1 decp1 stm1)

-- TODO
callStm :: Parser Stm
callStm = do
    rword "call"
    pname1 <- pname
    return (Call pname1)

testString = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"

main = putStrLn "Hello, World!"

parse :: String -> Stm
parse (str) = Skip
