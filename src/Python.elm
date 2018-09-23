module Python exposing (code)

import Code

nameFromComponents : List (List String) -> String
nameFromComponents ns =
  let
    each n =
      case List.map (String.filter Char.isAlphaNum) n of
        [] -> "None"
        first :: rest ->
          let
              titleCase str =
                case String.uncons str of
                  Nothing -> ""
                  Just (s, tr) -> String.cons (Char.toUpper s) tr
          in
          String.toLower first :: List.map titleCase rest
          |> String.concat
  in
  String.join "." (List.map each ns)

name : Code.Name -> String
name n =
  case n of
    Code.Var css -> nameFromComponents css
    Code.SelfDot css -> nameFromComponents (["self"] :: css)

call : Code.Call -> String
call (Code.Call n args) =
  name n ++ "(" ++ String.join ", " (List.map expr args) ++ ")"

expr : Code.Expr -> String
expr e =
  case e of
    Code.Value n -> name n
    Code.ExprCall c -> call c
    Code.Bool True -> "True"
    Code.Bool False -> "False"

condition : Code.Condition -> String
condition cond =
  let
      op nOp conds =
        List.intersperse nOp (List.map condition conds)
        |> String.join " "
  in
  case cond of
    Code.CondExpr e -> expr e
    Code.Equal True  e1 e2 -> expr e1 ++ " == " ++ expr e2
    Code.Equal False e1 e2 -> expr e1 ++ " != " ++ expr e2
    Code.CondOp Code.And conds -> op "and" conds
    Code.CondOp Code.Or  conds -> op "or" conds
    Code.Not (Code.CondExpr e) -> "not " ++ expr e
    Code.Not c -> "not (" ++ condition c ++ ")"

stmt : Code.Stmt -> String
stmt st =
  case st of
    Code.StmtCall c -> call c
    Code.Pass -> "pass"
    Code.If cond then_ ->
      "if " ++ condition cond ++ ":\n"
      ++ String.join "\n" (List.map (\s -> "    " ++ stmt s) then_)

code : Code.Code -> String
code stmts = String.join "\n" (List.map stmt stmts)
