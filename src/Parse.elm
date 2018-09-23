module Parse exposing (parse)

import Dict

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

breakList : (a -> Maybe b) -> List a -> Maybe (List a, b, List a)
breakList f xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      case f x of
        Just b -> Just ([], b, rest)
        Nothing ->
          Maybe.map
            (\(before, at, after) -> (x :: before, at, after))
            (breakList f rest)

breakList_ : (a -> Bool) -> List a -> Maybe (List a, a, List a)
breakList_ p = breakList (\x -> if p x then Just x else Nothing)

shatterList_ : (a -> Bool) -> List a -> List (List a)
shatterList_ p xs =
  case breakList_ p xs of
    Nothing -> [xs]
    Just (before, _, after) ->
      before :: shatterList_ p after

var : Sentence.Fragment -> Code.Name
var frag =
  case frag of
    "I" :: rest -> Code.SelfDot rest
    _ -> Code.Var frag

call : Sentence.Fragment -> Code.Call
call frag =
  let
      (con, rest) =
        case frag of
          "I" :: r -> (Code.SelfDot, r)
          _ -> (Code.Var, frag)
      noArg = Code.Call (con rest) []
  in
  case shatterList_ (\w -> w == "I") rest of
    [] -> noArg
    [_] -> noArg
    r :: rs -> Code.Call (con r) (List.map (Code.Value << Code.SelfDot) rs)

conditionExpr : Sentence.Fragment -> Code.Condition
conditionExpr frag =
  let
      expandContractedEquality w =
        if String.right 3 w == "'re"
        then Just (String.dropRight 3 w)
        else if String.right 2 w == "'m"
        then Just (String.dropRight 2 w)
        else Nothing
      equalities =
        Dict.fromList
          [ ("is",  False)
          , ("are", False)
          , ("am",  False)
          , ("isn't",  True)
          , ("aren't", True)
          ]
      findEquality word =
        case Dict.get word equalities of
          Just negated -> Just (Nothing, negated)
          Nothing ->
            case expandContractedEquality word of
              Just prevWord -> Just (Just [prevWord], False)
              Nothing -> Nothing
  in
  case breakList findEquality frag of
    Nothing ->
      if List.isEmpty frag
      then Code.CondExpr (Code.Bool True)
      else Code.CondExpr (Code.ExprCall (call frag))
    Just (left, (leftSuffix, negated), right) ->
      Code.Equal (not negated)
        (Code.Value (var (left ++ Maybe.withDefault [] leftSuffix)))
        (Code.Value (var right))

conditionFrag : Sentence.Fragment -> Code.Condition
conditionFrag frag =
  case shatterList_ (\w -> w == "and") frag of
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
                  else (Code.StmtCall (call f), fs)
        in
        prependInteresting
          [Code.If condResult.interesting [stmt]]
          (parse unparsed)
      else
        prependBoring (word :: fws) (parse frags)
