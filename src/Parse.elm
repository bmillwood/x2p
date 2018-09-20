module Parse exposing (parse)

import Set

import Code
import Sentence

type alias Result a =
  { interesting : a
  , thenBoring : List Sentence.Fragment
  }

type alias Results a =
  { before : List Sentence.Fragment
  , results : List (Result a)
  }

emptyResults : Results a
emptyResults = { before = [], results = [] }

prependBoring : Sentence.Fragment -> Results a -> Results a
prependBoring boringFrag { before, results } =
  { before = boringFrag :: before, results = results }

prependInteresting : a -> Results a -> Results a
prependInteresting a { before, results } =
  { before = []
  , results = { interesting = a, thenBoring = before } :: results
  }

breakList : (a -> Bool) -> List a -> (List a, List a)
breakList p xs =
  case xs of
    [] -> ([], [])
    x :: rest ->
      if p x
      then ([], xs)
      else
        let
            (before, after) = breakList p rest
        in
            (x :: before, after)

shatterList : (a -> Bool) -> List a -> List (List a)
shatterList p xs =
  case breakList p xs of
    (left, []) -> [left]
    (left, _ :: right) -> left :: shatterList p right

var : Sentence.Fragment -> Code.Name
var frag =
  case frag of
    "I" :: rest -> Code.SelfDot rest
    _ -> Code.Var frag

conditionExpr : Sentence.Fragment -> Code.Condition
conditionExpr frag =
  let
      expandContraction w =
        if String.right 3 w == "'re"
        then [String.dropRight 3 w, "are"]
        else if String.right 2 w == "'m"
        then [String.dropRight 2 w, "am"]
        else [w]
      expFrag = List.concatMap expandContraction frag
      equalities = Set.fromList [ "is", "are", "am" ]
  in
  case breakList (\w -> Set.member w equalities) expFrag of
    ([], []) -> Code.CondExpr (Code.Bool True)
    (_, []) ->
      Code.CondExpr (Code.ExprCall (Code.Call (var frag) []))
    (left, _ :: right) ->
      Code.Equal
        (Code.Value (var left))
        (Code.Value (var right))

conditionFrag : Sentence.Fragment -> Code.Condition
conditionFrag frag =
  case shatterList (\w -> w == "and") frag of
    [] -> Code.CondExpr (Code.Bool True)
    [one] -> conditionExpr one
    conjuncts -> Code.And (List.map conditionExpr conjuncts)

condition
  : Sentence.Fragment
 -> List Sentence.Fragment
 -> Result Code.Condition
condition thisFrag remainingFrags =
  { interesting = conditionFrag thisFrag
  , thenBoring = remainingFrags
  }

parse : List Sentence.Fragment -> Results Code.Code
parse fragments =
  case fragments of
    [] -> emptyResults
    [] :: frags -> parse frags
    (word :: fws) :: frags ->
      if String.toLower word == "if"
      then 
        let
            condResult = condition fws frags
            dropInitialThen fs =
              case fs of
                [] -> []
                [] :: r -> dropInitialThen r
                ("then" :: f) :: r -> f :: r
                _ -> fs
            (stmt, unparsed) =
              case dropInitialThen condResult.thenBoring of
                [] -> (Code.Pass, [])
                f :: fs ->
                  if List.all String.isEmpty f
                  then (Code.Pass, fs)
                  else (Code.StmtCall (Code.Call (var f) []), fs)
        in
        prependInteresting
          [Code.If condResult.interesting [stmt]]
          (parse unparsed)
      else
        prependBoring (word :: fws) (parse frags)
