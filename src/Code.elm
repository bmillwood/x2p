module Code exposing (..)

import Sentence

type alias Name = Sentence.Fragment

type Call = Call Name (List Expr)

type Expr
  = ExprCall Call
  | Value Name

type Condition
  = CondAtom Expr
  | Equal Expr Expr
  | And (List Condition)
  | Or (List Condition)

type Stmt
  = StmtCall Call
  | Pass
  | If Condition Code

type alias Code = List Stmt
