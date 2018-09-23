module Cpp exposing (code)

import Code

nameFromComponents : List (List String) -> String
nameFromComponents ns =
  let
    each n =
      case List.map (String.filter Char.isAlphaNum) n of
        [] -> "/* ?? */"
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
    Code.SelfDot css -> nameFromComponents (["this"] :: css)

call : Code.Call -> String
call (Code.Call n args) =
  name n ++ "(" ++ String.join ", " (List.map expr args) ++ ")"

expr : Code.Expr -> String
expr e =
  case e of
    Code.Value n -> name n
    Code.ExprCall c -> call c
    Code.Bool True -> "true"
    Code.Bool False -> "false"

type CondNF
  = CondExpr Bool Code.Expr
  | Equal Bool Code.Expr Code.Expr
  | CondOp Code.CondOp (List CondNF)

pushNots : Bool -> Code.Condition -> CondNF
pushNots negated c =
  case c of
    Code.CondExpr e -> CondExpr negated e
    Code.Equal b e1 e2 -> Equal (b /= negated) e1 e2
    Code.CondOp op conds ->
      let
          newOp =
            case (negated, op) of
              (False, _) -> op
              (True, Code.Or)  -> Code.And
              (True, Code.And) -> Code.Or
      in
      CondOp newOp (List.map (pushNots negated) conds)
    Code.Not notC -> pushNots (not negated) notC

nfCondition : CondNF -> String
nfCondition cond =
  case cond of
    CondExpr negated e -> (if negated then "!" else "") ++ expr e
    Equal b e1 e2 -> expr e1 ++ (if b then " == " else " != ") ++ expr e2
    CondOp op conds ->
      let
          nOp =
            case op of
              Code.Or -> "||"
              Code.And -> "&&"
      in
      List.intersperse nOp (List.map nfCondition conds)
      |> String.join " "

condition : Code.Condition -> String
condition = nfCondition << pushNots False

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
