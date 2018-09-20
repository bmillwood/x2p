module Parse exposing (parse)

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

condition
  : Sentence.Fragment
 -> List Sentence.Fragment
 -> Result Code.Condition
condition thisFrag remainingFrags =
  { interesting = Code.CondAtom (Code.ExprCall thisFrag)
  , thenBoring = remainingFrags
  }

parse : Sentence.Sentence -> Results Code.Code
parse fragments =
  case fragments of
    [] -> emptyResults
    [] :: frags -> parse frags
    (word :: fws) :: frags ->
      if String.toLower word == "if"
      then 
        let
            condResult = condition fws frags
            (stmt, unparsed) =
              case condResult.thenBoring of
                [] -> (Code.Pass, [])
                f :: fs -> (Code.StmtCall f, fs)
        in
        prependInteresting
          [Code.If condResult.interesting [stmt]]
          (parse unparsed)
      else
        prependBoring (word :: fws) (parse frags)
