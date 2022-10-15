module Interpreter where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.Map as Map
import Parser.AST
import Utils

data EnvEntry
  = MInterpFunc FuncName [IdentName] [Statement]
  | MInterpInt Int
  | MInterpStr String
  | MInterpNull
  | MInterpBool Bool

newtype Environment =
  Environment
    { getEnv :: Map.Map String EnvEntry
    }

data InterpError
  = NoError
  | DivisionByZero
  deriving (Show)

type Interpreter a = EitherT InterpError (StateT Environment IO) a

throw :: Monad m => InterpError -> EitherT InterpError m a
throw = EitherT . pure . Left

yield :: a -> Interpreter a
yield = pure

evalAtom :: Atom -> Interpreter Atom
evalAtom = yield

atomBinOp :: Atom -> Operator -> Atom -> Interpreter Atom
atomBinOp a1 op a2 =
  case (a1, op, a2) of
    (MInt n1, OpMultiply, MInt n2) -> yield $ MInt (n1 * n2)
    (MInt n1, OpDivide, MInt 0) -> throw DivisionByZero
    (MInt n1, OpDivide, MInt n2) -> yield $ MInt (n1 `div` n2)
    _ -> throw NoError

evalFactors :: Factors -> Interpreter Atom
evalFactors (Factors first la) =
  foldM (\a1 (op, a2) -> atomBinOp a1 op a2) first la

factorsBinOp :: Factors -> Operator -> Factors -> Interpreter Atom
factorsBinOp f1 op f2 = do
  a1 <- evalFactors f1
  a2 <- evalFactors f2
  case (a1, op, a2) of
    (MInt n1, OpPlus, MInt n2) -> yield $ MInt (n1 + n2)
    (MInt n1, OpMinus, MInt n2) -> yield $ MInt (n1 - n2)
    (MStr s1, OpPlus, MStr s2) -> yield $ MStr (s1 <> s2)
    _ -> throw NoError
