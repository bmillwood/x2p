module Cpp exposing (code)

import Code

nameFromComponents : List String -> String
nameFromComponents n =
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

name : Code.Name -> String
name n =
  case n of
    Code.Var css -> String.join "." (List.map nameFromComponents css)
    Code.SelfDot cs -> "this." ++ nameFromComponents cs

call : Code.Call -> String
call (Code.Call n args) =
  name n ++ "(" ++ String.join ", " (List.map expr args) ++ ")"

expr : Code.Expr -> String
expr e =
  case e of
    Code.Value n -> name n
    Code.ExprCall c -> call c
    Code.Bool True -> "1"
    Code.Bool False -> "0"

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
    Code.And conds -> op "&&" conds
    Code.Or conds -> op "||" conds

stmt : Code.Stmt -> String
stmt st =
  case st of
    Code.StmtCall c -> call c
    Code.Pass -> "pass"
    Code.If cond then_ ->
      "if(" ++ condition cond ++ ") {\n"
      ++ String.concat (List.map (\s -> "    " ++ stmt s ++ ";\n") then_)
      ++ "}"

code : Code.Code -> String
code stmts = String.join "\n" (List.map stmt stmts)
