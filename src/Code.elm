module Code exposing (..)

type Name
  = Var (List (List String))
  | SelfDot (List (List String))

type Call = Call Name (List Expr)

type Expr
  = ExprCall Call
  | Value Name
  | Bool Bool

type CondOp = And | Or

type Condition
  = CondExpr Expr
  | Equal Bool Expr Expr
  | CondOp CondOp (List Condition)
  | Not Condition

type Stmt
  = StmtCall Call
  | Pass
  | If Condition Code

type alias Code = List Stmt
