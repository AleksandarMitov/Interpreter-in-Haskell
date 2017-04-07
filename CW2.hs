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

testString = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"

main = putStrLn "Hello, World!"

parse :: String -> Stm
parse (str) = Skip
