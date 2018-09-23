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

var : List Sentence.Fragment -> Code.Name
var frags =
  case frags of
    ("I" :: rest) :: others -> Code.SelfDot (rest :: others)
    _ -> Code.Var frags

call : Sentence.Fragment -> Code.Call
call frag =
  let
      (con, rest) =
        case frag of
          "I" :: r -> (Code.SelfDot, r)
          _ -> (Code.Var, frag)
      noArg = Code.Call (con [rest]) []
  in
  case shatterList_ (\w -> w == "I") rest of
    [] -> noArg
    [_] -> noArg
    r :: rs ->
      Code.Call
        (con [r])
        (List.map
          (\n -> Code.Value (Code.SelfDot [n]))
          rs)

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
      findNot word =
        if word == "not"
        then Just []
        else if String.right 3 word == "n't"
        then Just [String.dropRight 3 word]
        else Nothing
  in
  case breakList findEquality frag of
    Nothing ->
      case breakList findNot frag of
        Nothing ->
          if List.isEmpty frag
          then Code.CondExpr (Code.Bool True)
          else Code.CondExpr (Code.ExprCall (call frag))
        Just (beforeNot, maybeWord, afterNot) ->
          let
              object = beforeNot
              method = maybeWord ++ afterNot
              callexpr = Code.ExprCall (Code.Call (var [object, method]) [])
          in
          Code.Not (Code.CondExpr callexpr)
    Just (left, (leftSuffix, negated), right) ->
      Code.Equal (not negated /= (List.head right == Just "not"))
        (Code.Value (var [left ++ Maybe.withDefault [] leftSuffix]))
        (Code.Value (var [right]))

conditionFrag : Sentence.Fragment -> Code.Condition
conditionFrag frag =
  let
      op opName opCon opZero k subFrag =
        case shatterList_ (\w -> w == opName) subFrag of
          [] -> Code.CondExpr (Code.Bool opZero)
          [one] -> k one
          many -> opCon (List.map k many)
  in
  op "or" (Code.CondOp Code.Or) False
    (op "and" (Code.CondOp Code.And) True conditionExpr)
    frag

condition
  : Sentence.Fragment
 -> List Sentence.Fragment
 -> Result Code.Condition
condition thisFrag remainingFrags =
  case breakList_ (\w -> w == "then") thisFrag of
    Nothing ->
      { interesting = conditionFrag thisFrag
      , thenBoring = remainingFrags
      }
    Just (cond, _, body) ->
      { interesting = conditionFrag cond
      , thenBoring = body :: remainingFrags
      }

parse : List Sentence.Fragment -> Results Code.Code
parse fragments =
  case fragments of
    [] -> emptyResults
    [] :: frags -> parse frags
    frag :: frags ->
      case breakList_ (\word -> String.toLower word == "if") frag of
        Nothing -> prependBoring frag (parse frags)
        Just ([], _, cond) ->
          let
              condResult = condition cond frags
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
        Just (before, _, cond) ->
          prependInteresting
            [Code.If (conditionFrag cond) [Code.StmtCall (call before)]]
            (parse frags)
