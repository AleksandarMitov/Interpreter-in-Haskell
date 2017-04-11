module Main where
import Control.Applicative
import Prelude hiding (Num)
import Control.Monad (void)
import Data.List (intercalate)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

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
rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","call", "proc", "is", "begin", "end", "var"]

-- handling whitespace and comments
sc :: Parser ()
sc = (L.space (void spaceChar) lineCmnt blockCmnt)
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

-- wrapper for the space consumer
lexeme :: Parser a -> Parser a
lexeme = L.lexeme  (sc)

--Since we often want to parse some “fixed” string, let’s define one more parser called symbol.
--It will take a string as argument and parse this string and whitespace after it.
symbol :: String -> Parser String
symbol = L.symbol ( sc)

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = (between (symbol "(") (symbol ")"))

-- | 'num' parses an integer.
num :: Parser Integer
num = lexeme L.integer

rword :: String -> Parser ()
rword w = (string w *> notFollowedBy alphaNumChar *> sc)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
--END UTILITY STUFF

-- TODO
decv :: Parser DecV
decv = many (do
    rword "var"
    name  <- identifier
    symbol ":="
    aexp1 <- aexp
    symbol ";"
    return (name, aexp1))

-- TODO
decp :: Parser DecP
decp = dbg "decp" (many (do
    dbg "decp rword" (rword "proc")
    name <- identifier
    rword "is"
    stm1 <- dbg "decp stm1" (try stmsub <|> (parens stm))
    symbol ";"
    return (name, stm1)))

aexp :: Parser Aexp
aexp = (makeExprParser aTerm aOperators)

bexp :: Parser Bexp
bexp =  (makeExprParser bTerm bOperators)

aOperators :: [[Operator Parser Aexp]]
aOperators =
  [
    [ InfixL (Mult <$ symbol "*")]
  , [ InfixL (Add  <$ symbol "+")
    , InfixL (Sub  <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser Bexp]]
bOperators =
  [ [Prefix (Neg <$ symbol "!") ]
  , [InfixL (And <$ symbol "&")]
  ]

cOperators :: [[Operator Parser Stm]]
cOperators =
    [ [InfixR (Comp <$ symbol ";") ]
    ]

aTerm :: Parser Aexp
aTerm = parens aexp
  <|> V      <$> identifier
  <|> N      <$> num

bTerm :: Parser Bexp
bTerm =  parens bexp
    <|> try (rword "true"  *> pure (TRUE))
    <|> try (rword "false" *> pure (FALSE))
    <|> try le
    <|> try eq

stm :: Parser Stm
stm =  (makeExprParser cTerm cOperators)
--stm = (try compStm <|> try stmsub)

stmsub :: Parser Stm
stmsub = dbg "stmsub" (blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> try assStm)

cTerm :: Parser Stm
cTerm = dbg "cTerm" (parens stm <|> blockStm <|> ifStm <|> whileStm <|> skipStm <|> callStm <|> try assStm)

le :: Parser Bexp
le = do
    a1 <- aexp
    symbol "<="
    a2 <- aexp
    return (Le a1 a2)

eq :: Parser Bexp
eq = (do
    a1 <- aexp
    symbol "="
    a2 <- aexp
    return (Eq a1 a2))

ifStm :: Parser Stm
ifStm = dbg "ifStm" (do
    rword "if"
    cond  <- bexp
    rword "then"
    stmt1 <- stm
    rword "else"
    stmt2 <- stm
    return (If cond stmt1 stmt2))

whileStm :: Parser Stm
whileStm = dbg "whileStm" (do
    rword  "while"
    cond   <- bexp
    rword  "do"
    stmt1  <- try (parens stm) <|> try stm
    return (While cond stmt1))

assStm :: Parser Stm
assStm = dbg "assStm" (do
    var  <- identifier
    symbol ":="
    expr <- aexp
    return (Ass var expr))

skipStm :: Parser Stm
skipStm = dbg "skipStm" (Skip <$ rword "skip")

-- TODO
compStm :: Parser Stm
compStm = dbg "compStm" (do
    stm1  <- stmsub
    symbol ";"
    stm2  <- stm
    return (Comp stm1 stm2))

-- TODO
blockStm :: Parser Stm
blockStm = dbg "blockStm" (do
    rword "begin"
    decv1 <- decv
    decp1 <- decp
    stm1 <- stm
    rword "end"
    return (Block decv1 decp1 stm1))

-- TODO
callStm :: Parser Stm
callStm = dbg "callStm" (do
    rword "call"
    pname1 <- identifier
    return (Call pname1))

testString = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"

main = putStrLn (show (parse testString))

parse :: String -> Stm
parse str = case (parseMaybe (between sc eof stm) str) of
    Just result -> result
    Nothing -> Skip

parseFile :: FilePath -> IO ()
parseFile filePath = do
  file <- readFile filePath
  putStrLn $ case parseMaybe (between sc eof stm) file of
    Nothing   -> show Skip
    Just prog -> show prog
