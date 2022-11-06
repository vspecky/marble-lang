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
      stdLookup = Map.lookup name stdFnDict
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
    MInt n _ -> pure $ MInterpInt n
    MStr s _ -> pure $ MInterpStr s
    MBool b _ -> pure $ MInterpBool b
    MNull _ -> pure MInterpNull
    _ ->
      throw $
      InterpreterErr $ "Value of '" <> show a <> "' cannot be stored in env"

atomAsBool :: Atom -> Interpreter Bool
atomAsBool a =
  case a of
    MInt n _ -> pure (n > 0)
    MStr s _ -> pure (s /= "")
    MBool b _ -> pure b
    MNull _ -> pure False
    _ -> throw $ InterpreterErr $ "Tried to convert '" <> show a <> "' to bool"

evalVarRef :: IdentName -> Interpreter Atom
evalVarRef varName = do
  entry <- envLookup varName
  case entry of
    MInterpInt n -> pure $ MInt n noPos
    MInterpStr s -> pure $ MStr s noPos
    MInterpBool b -> pure $ MBool b noPos
    MInterpNull -> pure $ MNull noPos
    MInterpFunc fnName _ _ ->
      throw $ RuntimeError $ "Function '" <> fnName <> "' never called"

evalFuncApplication :: FuncName -> [Expression] -> Interpreter Atom
evalFuncApplication fname exprs = do
  env <- lift getState
  let stdFn = Map.lookup fname stdFnDict
      langFn = Map.lookup fname (unEnv env)
  case (stdFn, langFn) of
    (Just stdF, Nothing) -> do
      args <- forM exprs evalExpression
      stdF args
    (Nothing, Just (MInterpFunc _ argNames body))
      | length argNames == length exprs -> do
        envExprs <- forM exprs evalExpression
        envEntries <- forM envExprs intoEnvEntry
        let fnEnvMap = Map.filter entryIsFunc (unEnv env)
            envMap = Map.fromList $ zip argNames envEntries
            env' = Environment $ Map.union envMap fnEnvMap
        args <- forM exprs evalExpression
        lift $ setState env'
        result <- evalBlock body
        lift $ setState env
        case result of
          Returning val -> pure val
          _ -> pure $ MNull noPos
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
    FuncApplication fnName fnArgs _ -> evalFuncApplication fnName fnArgs
    Identifier ident _ -> evalVarRef ident
    Nested expr _ -> evalExpression expr
    MInt n _ -> pure $ MInt n noPos
    MStr s _ -> pure $ MStr s noPos
    MBool b _ -> pure $ MBool b noPos
    MNull _ -> pure $ MNull noPos

atomBinOp :: Atom -> Operator -> Atom -> Interpreter Atom
atomBinOp a1 op a2 =
  case (a1, op, a2) of
    (MInt n1 _, OpPlus _, MInt n2 _) -> yield $ MInt (n1 + n2) noPos
    (MInt n1 _, OpMinus _, MInt n2 _) -> yield $ MInt (n1 - n2) noPos
    (MInt n1 _, OpMultiply _, MInt n2 _) -> yield $ MInt (n1 * n2) noPos
    (MInt n1 _, OpDivide _, MInt 0 _) -> throw DivisionByZero
    (MInt n1 _, OpDivide _, MInt n2 _) -> yield $ MInt (n1 `div` n2) noPos
    (MInt n1 _, OpEqual _, MInt n2 _) -> yield $ MBool (n1 == n2) noPos
    (MInt n1 _, OpGreaterThan _, MInt n2 _) -> yield $ MBool (n1 > n2) noPos
    (MInt n1 _, OpGreaterThanEq _, MInt n2 _) -> yield $ MBool (n1 >= n2) noPos
    (MInt n1 _, OpLesserThan _, MInt n2 _) -> yield $ MBool (n1 < n2) noPos
    (MInt n1 _, OpLesserThanEq _, MInt n2 _) -> yield $ MBool (n1 <= n2) noPos
    (MBool b1 _, OpBoolAnd _, MBool b2 _) -> yield $ MBool (b1 && b2) noPos
    (MBool b1 _, OpBoolOr _, MBool b2 _) -> yield $ MBool (b1 || b2) noPos
    (MStr s1 _, OpPlus _, MStr s2 _) -> yield $ MStr (s1 <> s2) noPos
    (MStr s1 _, OpEqual _, MStr s2 _) -> yield $ MBool (s1 == s2) noPos
    (fa@(FuncApplication {}), op, other) -> evalThenBinOp fa op other
    (other, op, fa@(FuncApplication {})) -> evalThenBinOp other op fa
    (ident@(Identifier {}), op, other) -> evalThenBinOp ident op other
    (other, op, ident@(Identifier {})) -> evalThenBinOp other op ident
    (Nested expr _, op, other) -> do
      a1' <- evalExpression expr
      atomBinOp a1' op other
    (other, op, Nested expr _) -> do
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
evalFactors (Factors first [] _) = evalAtom first
evalFactors (Factors first rest _) =
  foldM (\a1 (op, a2) -> atomBinOp a1 op a2) first rest

atomFactorsBinOp :: Atom -> Operator -> Factors -> Interpreter Atom
atomFactorsBinOp a1 op f = do
  a2 <- evalFactors f
  atomBinOp a1 op a2

evalTerms :: Terms -> Interpreter Atom
evalTerms (Terms first rest _) = do
  atomFirst <- evalFactors first
  foldM (\a (op, f) -> atomFactorsBinOp a op f) atomFirst rest

evalComparison :: Comparison -> Interpreter Atom
evalComparison (Comparison t Nothing _) = evalTerms t
evalComparison (Comparison t1 (Just (op, t2)) _) = do
  a1 <- evalTerms t1
  a2 <- evalTerms t2
  atomBinOp a1 op a2

atomComparisonBinOp :: Atom -> Operator -> Comparison -> Interpreter Atom
atomComparisonBinOp a1 op c = do
  a2 <- evalComparison c
  atomBinOp a1 op a2

evalExpression :: Expression -> Interpreter Atom
evalExpression (Expression first rest _) = do
  a1 <- evalComparison first
  foldM (\a (op, c) -> atomComparisonBinOp a op c) a1 rest

evalStatement :: Statement -> Interpreter ExecutionStatus
evalStatement stmt =
  case stmt of
    Return expr _ -> do
      a <- evalExpression expr
      pure $ Returning a
    VarDecl varName expr _ -> do
      a <- evalExpression expr
      entry <- intoEnvEntry a
      envSetNew varName entry
      pure Executing
    VarMutate varName expr _ -> do
      a <- evalExpression expr
      entry <- intoEnvEntry a
      envSet varName entry
      pure Executing
    If expr ifBlock elseCase _ -> do
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
    FuncDecl fName args body _ -> envSetNew fName (MInterpFunc fName args body)
    TLExpr expr _ -> evalExpression expr >>= const (pure ())
    TLStatement stmt _ -> evalStatement stmt >>= const (pure ())

evalProgram :: Program -> Interpreter ()
evalProgram (Program topLevels) = do
  fnLessTopLevels <- buildFnEnv topLevels
  forM_ fnLessTopLevels evalTopLevel
  where
    buildFnEnv :: [TopLevel] -> Interpreter [TopLevel]
    buildFnEnv [] = pure []
    buildFnEnv (t:ts) =
      case t of
        FuncDecl fName args body _ -> do
          envSetNew fName (MInterpFunc fName args body)
          buildFnEnv ts
        _ -> do
          rest <- buildFnEnv ts
          pure (t : rest)
