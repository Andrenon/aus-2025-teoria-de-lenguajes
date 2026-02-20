module AST where

import Data.Map.Strict (Map)

type Variable = String

data Value
  = IntVal Int
  | StrVal String
  deriving (Eq, Show)

-- Integer expressions
data IntExp
  = Const Int
  | Var Variable
  | UMinus IntExp
  | Plus IntExp IntExp
  | Minus IntExp IntExp
  | Times IntExp IntExp
  | Div IntExp IntExp
  | Len StrExp               -- len(strexp)
  | ToInt StrExp             -- toInt(strexp)
  deriving (Eq, Show)

-- String expressions
data StrExp
  = StrLit String
  | SVar Variable
  | Concat StrExp StrExp     -- str1 ++ str2
  | PipeStr StrExp StrFilter -- strexp |> filter
  | ToStr IntExp             -- toStr(intexp)
  deriving (Eq, Show)

data StrFilter = Upper | Lower | Reverse | Trim
  deriving (Eq, Show)

-- Boolean expressions
data BoolExp
  = BTrue
  | BFalse
  | Eq IntExp IntExp
  | Lt IntExp IntExp
  | Gt IntExp IntExp
  | And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Not BoolExp
  deriving (Eq, Show)

-- Commands
data Comm
  = Skip
  | LetInt Variable IntExp     -- var := intexp
  | LetStr Variable StrExp     -- svar ::= strexp  (nuevo)
  | Seq Comm Comm              -- c1 ; c2
  | Cond BoolExp Comm Comm     -- if b then c else c end
  | Repeat Comm BoolExp        -- repeat c until b end
  | Step StrExp                -- step(strexp)     (nuevo)
  | Pipe Comm PipeAction       -- comm |> pipe_action (nuevo)
  deriving (Eq, Show)

data PipeAction
  = ActFilter StrFilter        -- upper/lower/reverse/trim
  | ActPrint                   -- print
  | ActSleep IntExp            -- sleep(intexp)
  deriving (Eq, Show)

type Env = Map Variable Value
