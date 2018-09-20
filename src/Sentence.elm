module Sentence exposing
  ( Sentence, Word, Fragment
  , tokenize
  )

type alias Word = String
type alias Fragment = List Word
type alias Sentence = List Fragment

splits : List String -> String -> List String
splits seps input =
  case seps of
    [] ->
      [input]
    sep :: remainingSeps ->
      List.concatMap (splits remainingSeps) (String.split sep input)

tokenize : String -> List Sentence
tokenize contents =
  splits ["!", ".", "?"] contents
  |> List.map (List.map String.words << String.split ",")
  |> List.filter (not << List.all (List.all String.isEmpty))
