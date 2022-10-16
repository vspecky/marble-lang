module Interpreter where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Interpreter.Environment
import Interpreter.Error
import Interpreter.Std
import Parser.AST
import Utils

data ExecutionStatus
  = Executing
  | EndOfBlock
  | Returning Atom
  deriving (Show)

yield :: a -> Interpreter a
yield = pure

envSetNew :: String -> EnvEntry -> Interpreter ()
envSetNew name entry = do
  env <- lift getState
  let langLookup = Map.lookup name (unEnv env)
  let stdLookup = Map.lookup name stdFnDict
  if isJust langLookup || isJust stdLookup
    then throw $
         RuntimeError $
         "Identifier '" <> name <> "' already exists in current scope"
    else do
      let newEntries = Map.insert name entry (unEnv env)
      lift $ setState $ Environment newEntries

envSet :: String -> EnvEntry -> Interpreter ()
envSet name entry = do
  env <- lift getState
  let langLookup = Map.lookup name (unEnv env)
  if isNothing langLookup
    then throw $ NotFoundInScope name
    else do
      let newEntries = Map.insert name entry (unEnv env)
      lift $ setState $ Environment newEntries

envLookup :: String -> Interpreter EnvEntry
envLookup ident = do
  env <- lift getState
  case Map.lookup ident (unEnv env) of
    Just entry -> pure entry
    Nothing -> throw (NotFoundInScope ident)

intoEnvEntry :: Atom -> Interpreter EnvEntry
intoEnvEntry a =
  case a of
    MInt n -> pure $ MInterpInt n
    MStr s -> pure $ MInterpStr s
    MBool b -> pure $ MInterpBool b
    MNull -> pure MInterpNull
    _ ->
      throw $
      InterpreterErr $ "Value of '" <> show a <> "' cannot be stored in env"

atomAsBool :: Atom -> Interpreter Bool
atomAsBool a =
  case a of
    MInt n -> pure (n > 0)
    MStr s -> pure (s /= "")
    MBool b -> pure b
    MNull -> pure False
    _ -> throw $ InterpreterErr $ "Tried to convert '" <> show a <> "' to bool"

evalVarRef :: IdentName -> Interpreter Atom
evalVarRef varName = do
  entry <- envLookup varName
  case entry of
    MInterpInt n -> pure $ MInt n
    MInterpStr s -> pure $ MStr s
    MInterpBool b -> pure $ MBool b
    MInterpNull -> pure MNull
    MInterpFunc fnName _ _ ->
      throw $ RuntimeError $ "Function '" <> fnName <> "' never called"

evalFuncApplication :: FuncName -> [Expression] -> Interpreter Atom
evalFuncApplication fname exprs = do
  env <- lift getState
  let stdFn = Map.lookup fname stdFnDict
  let langFn = Map.lookup fname (unEnv env)
  case (stdFn, langFn) of
    (Just stdF, Nothing) -> do
      args <- forM exprs evalExpression
      stdF args
    (Nothing, Just (MInterpFunc _ argNames body))
      | length argNames == length exprs -> do
        envExprs <- forM exprs evalExpression
        envEntries <- forM envExprs intoEnvEntry
        let fnEnvMap = Map.filter entryIsFunc (unEnv env)
        let envMap = Map.fromList $ zip argNames envEntries
        let env' = Environment $ Map.union envMap fnEnvMap
        args <- forM exprs evalExpression
        lift $ setState env'
        result <- evalBlock body
        lift $ setState env
        case result of
          Returning val -> pure val
          _ -> pure MNull
      | otherwise ->
        throw $
        RuntimeError $
        "Invalid number of arguments to function '" <> fname <> "'"
    (Nothing, Just _) ->
      throw $ RuntimeError $ "'" <> fname <> "' is not a function"
    _ ->
      throw $ RuntimeError $ "Identifier '" <> fname <> "' not found in scope"
  where
    entryIsFunc :: EnvEntry -> Bool
    entryIsFunc MInterpFunc {} = True
    entryIsFunc _ = False

evalAtom :: Atom -> Interpreter Atom
evalAtom a =
  case a of
    FuncApplication fnName fnArgs -> evalFuncApplication fnName fnArgs
    Identifier ident -> evalVarRef ident
    Nested expr -> evalExpression expr
    MInt n -> pure $ MInt n
    MStr s -> pure $ MStr s
    MBool b -> pure $ MBool b
    MNull -> pure MNull

atomBinOp :: Atom -> Operator -> Atom -> Interpreter Atom
atomBinOp a1 op a2 =
  case (a1, op, a2) of
    (MInt n1, OpPlus, MInt n2) -> yield $ MInt (n1 + n2)
    (MInt n1, OpMinus, MInt n2) -> yield $ MInt (n1 - n2)
    (MInt n1, OpMultiply, MInt n2) -> yield $ MInt (n1 * n2)
    (MInt n1, OpDivide, MInt 0) -> throw DivisionByZero
    (MInt n1, OpDivide, MInt n2) -> yield $ MInt (n1 `div` n2)
    (MInt n1, OpEqual, MInt n2) -> yield $ MBool (n1 == n2)
    (MInt n1, OpGreaterThan, MInt n2) -> yield $ MBool (n1 > n2)
    (MInt n1, OpGreaterThanEq, MInt n2) -> yield $ MBool (n1 >= n2)
    (MInt n1, OpLesserThan, MInt n2) -> yield $ MBool (n1 < n2)
    (MInt n1, OpLesserThanEq, MInt n2) -> yield $ MBool (n1 <= n2)
    (MBool b1, OpBoolAnd, MBool b2) -> yield $ MBool (b1 && b2)
    (MBool b1, OpBoolOr, MBool b2) -> yield $ MBool (b1 || b2)
    (MStr s1, OpPlus, MStr s2) -> yield $ MStr (s1 <> s2)
    (MStr s1, OpEqual, MStr s2) -> yield $ MBool (s1 == s2)
    (fa@(FuncApplication _ _), op, other) -> evalThenBinOp fa op other
    (other, op, fa@(FuncApplication _ _)) -> evalThenBinOp other op fa
    (ident@(Identifier _), op, other) -> evalThenBinOp ident op other
    (other, op, ident@(Identifier _)) -> evalThenBinOp other op ident
    (Nested expr, op, other) -> do
      a1' <- evalExpression expr
      atomBinOp a1' op other
    (other, op, Nested expr) -> do
      a2' <- evalExpression expr
      atomBinOp other op a2'
    (_, op, _) ->
      throw $
      InterpreterErr $
      "Operator '" <> show op <> "' not expected in atom binary op"
  where
    evalThenBinOp :: Atom -> Operator -> Atom -> Interpreter Atom
    evalThenBinOp a1 op a2 = do
      a1' <- evalAtom a1
      a2' <- evalAtom a2
      atomBinOp a1' op a2'

evalFactors :: Factors -> Interpreter Atom
evalFactors (Factors first []) = evalAtom first
evalFactors (Factors first rest) =
  foldM (\a1 (op, a2) -> atomBinOp a1 op a2) first rest

atomFactorsBinOp :: Atom -> Operator -> Factors -> Interpreter Atom
atomFactorsBinOp a1 op f = do
  a2 <- evalFactors f
  atomBinOp a1 op a2

evalTerms :: Terms -> Interpreter Atom
evalTerms (Terms first rest) = do
  atomFirst <- evalFactors first
  foldM (\a (op, f) -> atomFactorsBinOp a op f) atomFirst rest

evalComparison :: Comparison -> Interpreter Atom
evalComparison (Comparison t Nothing) = evalTerms t
evalComparison (Comparison t1 (Just (op, t2))) = do
  a1 <- evalTerms t1
  a2 <- evalTerms t2
  atomBinOp a1 op a2

atomComparisonBinOp :: Atom -> Operator -> Comparison -> Interpreter Atom
atomComparisonBinOp a1 op c = do
  a2 <- evalComparison c
  atomBinOp a1 op a2

evalExpression :: Expression -> Interpreter Atom
evalExpression (Expression first rest) = do
  a1 <- evalComparison first
  foldM (\a (op, c) -> atomComparisonBinOp a op c) a1 rest

evalStatement :: Statement -> Interpreter ExecutionStatus
evalStatement stmt =
  case stmt of
    Return expr -> do
      a <- evalExpression expr
      pure $ Returning a
    VarDecl varName expr -> do
      a <- evalExpression expr
      entry <- intoEnvEntry a
      envSetNew varName entry
      pure Executing
    VarMutate varName expr -> do
      a <- evalExpression expr
      entry <- intoEnvEntry a
      envSet varName entry
      pure Executing
    If expr ifBlock elseCase -> do
      a <- evalExpression expr
      aBool <- atomAsBool a
      case (aBool, elseCase) of
        (True, _) -> evalBlock ifBlock
        (False, Just elseBlock) -> evalBlock elseBlock
        (False, Nothing) -> pure Executing

evalBlock :: [Statement] -> Interpreter ExecutionStatus
evalBlock [] = pure EndOfBlock
evalBlock (x:xs) = do
  status <- evalStatement x
  case status of
    Returning value -> pure $ Returning value
    EndOfBlock -> pure EndOfBlock
    Executing -> evalBlock xs

evalTopLevel :: TopLevel -> Interpreter ()
evalTopLevel lvl = do
  case lvl of
    FuncDecl fName args body -> envSetNew fName (MInterpFunc fName args body)
    TLExpr expr -> evalExpression expr >>= const (pure ())
    TLStatement stmt -> evalStatement stmt >>= const (pure ())

evalProgram :: Program -> Interpreter ()
evalProgram (Program topLevels) = do
  fnLessTopLevels <- buildFnEnv topLevels
  forM_ fnLessTopLevels evalTopLevel
  where
    buildFnEnv :: [TopLevel] -> Interpreter [TopLevel]
    buildFnEnv [] = pure []
    buildFnEnv (t:ts) =
      case t of
        FuncDecl fName args body -> do
          envSetNew fName (MInterpFunc fName args body)
          buildFnEnv ts
        _ -> do
          rest <- buildFnEnv ts
          pure (t : rest)
