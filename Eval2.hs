module Eval2 (eval, runProgram) where

import AST
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Data.Char (toUpper, toLower, isSpace)

------------------------------------------------------------
-- STORE (estado del lenguaje)
------------------------------------------------------------

type Store = [(Variable, Value)]

initStore :: Store
initStore = []

------------------------------------------------------------
-- LOG Y PIPE
------------------------------------------------------------

data PipeVal = PipeVal
  { pipeValue :: String
  , pipeStep  :: Int
  } deriving (Eq, Show)

type TraceLog = [(Int, String)]

------------------------------------------------------------
-- MONADA DE EVALUACIÓN
------------------------------------------------------------

-- Semántica:
-- Comm × Store →
--   (Either Error (Maybe PipeVal), Store, TraceLog)

type Eval a =
  ExceptT String
    (StateT Store
      (Writer TraceLog)) a

------------------------------------------------------------
-- FUNCIONES AUXILIARES
------------------------------------------------------------

lookfor :: Variable -> Store -> Maybe Value
lookfor _ [] = Nothing
lookfor x ((y,v):ys)
  | x == y    = Just v
  | otherwise = lookfor x ys

update :: Variable -> Value -> Store -> Store
update x v [] = [(x,v)]
update x v ((y,w):ys)
  | x == y    = (x,v):ys
  | otherwise = (y,w) : update x v ys

lookupVar :: Variable -> Eval Value
lookupVar x = do
  st <- get
  case lookfor x st of
    Just v  -> return v
    Nothing -> throwError ("Variable no definida: " ++ x)

updateVar :: Variable -> Value -> Eval ()
updateVar x v = modify (update x v)

------------------------------------------------------------
-- EXPRESIONES ENTERAS
------------------------------------------------------------

evalInt :: IntExp -> Eval Int

evalInt (Const n) = return n

evalInt (Var x) = do
  v <- lookupVar x
  case v of
    IntVal n -> return n
    _        -> throwError "Se esperaba entero"

evalInt (UMinus e) =
  negate <$> evalInt e

evalInt (Plus a b) =
  (+) <$> evalInt a <*> evalInt b

evalInt (Minus a b) =
  (-) <$> evalInt a <*> evalInt b

evalInt (Times a b) =
  (*) <$> evalInt a <*> evalInt b

evalInt (Div a b) = do
  x <- evalInt a
  y <- evalInt b
  if y == 0
     then throwError "División por cero"
     else return (x `div` y)

evalInt (Len s) =
  length <$> evalStr s

evalInt (ToInt s) = do
  str <- evalStr s
  case reads str of
    [(n,"")] -> return n
    _        -> throwError "toInt: no es un entero válido"

------------------------------------------------------------
-- EXPRESIONES STRING
------------------------------------------------------------

evalStr :: StrExp -> Eval String

evalStr (StrLit s) = return s

evalStr (SVar x) = do
  v <- lookupVar x
  case v of
    StrVal s -> return s
    _        -> throwError "Se esperaba string"

evalStr (Concat a b) =
  (++) <$> evalStr a <*> evalStr b

evalStr (PipeStr s f) = do
  v <- evalStr s
  applyFilter f v

evalStr (ToStr e) =
  show <$> evalInt e

------------------------------------------------------------
-- FILTROS
------------------------------------------------------------

applyFilter :: StrFilter -> String -> Eval String
applyFilter Upper   = return . map toUpper
applyFilter Lower   = return . map toLower
applyFilter Reverse = return . reverse
applyFilter Trim    = return . filter (not . isSpace)

------------------------------------------------------------
-- EXPRESIONES BOOLEANAS
------------------------------------------------------------

evalBool :: BoolExp -> Eval Bool

evalBool BTrue  = return True
evalBool BFalse = return False

evalBool (Eq a b) =
  (==) <$> evalInt a <*> evalInt b

evalBool (Lt a b) =
  (<) <$> evalInt a <*> evalInt b

evalBool (Gt a b) =
  (>) <$> evalInt a <*> evalInt b

evalBool (And a b) =
  (&&) <$> evalBool a <*> evalBool b

evalBool (Or a b) =
  (||) <$> evalBool a <*> evalBool b

evalBool (Not a) =
  not <$> evalBool a

------------------------------------------------------------
-- COMANDOS
------------------------------------------------------------

evalComm :: Comm -> Eval (Maybe PipeVal)

evalComm Skip = return Nothing

evalComm (LetInt x e) = do
  n <- evalInt e
  updateVar x (IntVal n)
  return Nothing

evalComm (LetStr x e) = do
  s <- evalStr e
  updateVar x (StrVal s)
  return Nothing

evalComm (Seq c1 c2) = do
  _ <- evalComm c1
  evalComm c2

evalComm (Cond b ct ce) = do
  cond <- evalBool b
  if cond then evalComm ct
          else evalComm ce

evalComm (Repeat c b) =
  evalComm (Seq c (Cond b Skip (Repeat c b)))

------------------------------------------------------------
-- STEP Y PIPE
------------------------------------------------------------

evalComm (Step s) = do
  v <- evalStr s
  return (Just (PipeVal v 0))

evalComm (Pipe c action) = do
  result <- evalComm c
  case result of
    Nothing -> throwError "Pipe aplicado a comando sin flujo"
    Just pv -> applyPipeAction pv action

applyPipeAction :: PipeVal -> PipeAction -> Eval (Maybe PipeVal)

applyPipeAction (PipeVal v k) (ActFilter f) = do
  v' <- applyFilter f v
  return (Just (PipeVal v' (k+1)))

applyPipeAction (PipeVal v k) ActPrint = do
  tell [(k,v)]
  return (Just (PipeVal v (k+1)))

applyPipeAction (PipeVal v k) (ActSleep _) =
  return (Just (PipeVal v (k+1)))

------------------------------------------------------------
-- EVALUADOR PRINCIPAL
------------------------------------------------------------

eval :: Comm -> Eval (Maybe PipeVal)
eval = evalComm

runProgram ::
  Comm ->
  (Either String (Maybe PipeVal), Store, TraceLog)

runProgram p =
  let ((res, st), logg) =
        runWriter
          (runStateT
            (runExceptT (eval p))
            initStore)
  in (res, st, logg)
