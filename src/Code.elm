module Code exposing (..)

import Sentence

type alias Name = Sentence.Fragment

type Expr
  = ExprCall Name
  | Value Name

type Condition
  = CondAtom Expr

type Stmt
  = StmtCall Name
  | Pass
  | If Condition Code

type alias Code = List Stmt
