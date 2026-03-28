module Eval2 (runProgram) where

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

evalInt :: IntExp -> Eval Integer

evalInt (Const n) = return n

evalInt (Var x) = do
  v <- lookupVar x
  case v of
    IntVal n -> return n
    _        -> throwError "Se esperaba entero"

evalInt (UMinus e) = do val <- evalInt e
                        return (negate val)

evalInt (Plus l r) = do lval <- evalInt l
                        rval <- evalInt r
                        return (lval + rval)

evalInt (Minus l r) = do lval <- evalInt l
                         rval <- evalInt r
                         return (lval - rval)

evalInt (Times l r) = do lval <- evalInt l
                         rval <- evalInt r
                         return (lval * rval)

evalInt (Div l r) = do
  lval <- evalInt l
  rval <- evalInt r
  if rval == 0
     then throwError "División por cero"
     else return (div lval rval)

evalInt (Len s) = do str <- evalStr s
                     let longitud = length str
                     return (toInteger longitud)

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

evalStr (Concat a b) = do strA <- evalStr a
                          strB <- evalStr b
                          return (strA ++ strB)

evalStr (PipeStr s f) = do
  v <- evalStr s
  applyFilter f v

evalStr (ToStr e) = do n <- evalInt e
                       return (show n)

------------------------------------------------------------
-- FILTROS
------------------------------------------------------------

applyFilter :: StrFilter -> String -> Eval String
applyFilter Upper str   = return (map toUpper str)
applyFilter Lower str   = return (map toLower str)
applyFilter Reverse str = return (reverse str)
applyFilter Trim str    = return (filter (not . isSpace) str)


------------------------------------------------------------
-- EXPRESIONES BOOLEANAS
------------------------------------------------------------

evalBool :: BoolExp -> Eval Bool

evalBool BTrue  = return True
evalBool BFalse = return False

evalBool (Eq a b) = do valA <- evalInt a
                       valB <- evalInt b
                       return (valA == valB)

evalBool (Lt a b) = do valA <- evalInt a
                       valB <- evalInt b
                       return (valA < valB)

evalBool (Gt a b) = do valA <- evalInt a
                       valB <- evalInt b
                       return (valA > valB)

evalBool (And a b) = do valA <- evalBool a
                        valB <- evalBool b
                        return (valA && valB)

evalBool (Or a b) = do valA <- evalBool a
                       valB <- evalBool b
                       return (valA || valB)

evalBool (Not a) = do valA <- evalBool a
                      return (not valA)

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
  return (Just (PipeVal v (k)))

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
