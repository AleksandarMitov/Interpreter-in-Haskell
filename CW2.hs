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
type State = Var -> Z
type Z = Integer
type T = Bool

data Aexp = N Num | V Var | Mult Aexp Aexp
        | Add Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
        | Le Aexp Aexp | Eq Aexp Aexp
data Stm = Skip | Ass Var Aexp | Comp Stm Stm
        | If Bexp Stm Stm | While Bexp Stm
        | Block DecV DecP Stm | Call Pname

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

whileParser :: Parser Stm
whileParser = between sc eof stm

stm :: Parser Stm
stm = parens (ifStm <|> whileStm <|> skipStm <|> assStm <|> compStm <|> blockStm <|> callStm)

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x

ifStm :: Parser Stm
ifStm = do
    rword "if"
    cond  <- bExpr
    rword "then"
    stmt1 <- stm
    rword "else"
    stmt2 <- stm
    return (If cond stmt1 stmt2)

whileStm :: Parser Stm
whileStm = do
    rword "while"
    cond <- bExpr
    rword "do"
    stmt1 <- stm
    return (While cond stmt1)

assStm :: Parser Stm
assStm = do
    var  <- identifier
    void (symbol ":=")
    expr <- aExpr
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

testString = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"

main = putStrLn "Hello, World!"

parse :: String -> Stm
parse (str) = Skip
