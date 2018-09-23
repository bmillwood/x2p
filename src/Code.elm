module Code exposing (..)

type Name
  = Var (List String)
  | SelfDot (List String)

type Call = Call Name (List Expr)

type Expr
  = ExprCall Call
  | Value Name
  | Bool Bool

type Condition
  = CondExpr Expr
  | Equal Bool Expr Expr
  | And (List Condition)
  | Or (List Condition)

type Stmt
  = StmtCall Call
  | Pass
  | If Condition Code

type alias Code = List Stmt
