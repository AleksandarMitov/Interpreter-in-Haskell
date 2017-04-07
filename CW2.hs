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

testString = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"

main = putStrLn "Hello, World!"

parse :: String -> Stm
parse (str) = Skip
