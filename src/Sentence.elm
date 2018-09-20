module Sentence exposing
  ( Sentence, Word, Fragment, Terminator
  , tokenize, untokenize
  )

import Tuple

type alias Word = String
type alias Fragment = List Word
type alias Terminator = String
type alias Sentence = (List Fragment, Terminator)

splits : List String -> (String, Terminator) -> List (String, Terminator)
splits seps (input, t) =
  case seps of
    [] ->
      [(input, t)]
    sep :: remainingSeps ->
      String.split sep input
      |> List.foldr (\x (r, nt) -> ((x, nt) :: r, sep)) ([], t)
      |> Tuple.first
      |> List.concatMap (splits remainingSeps)

tokenize : String -> List Sentence
tokenize contents =
  splits ["!", ".", "?"] (contents, "")
  |> List.map (\(s, t) -> (List.map String.words (String.split "," s), t))
  |> List.filter (\(fs, t) -> not (List.all (List.all String.isEmpty) fs && String.isEmpty t))

untokenize : List Sentence -> String
untokenize sentences =
  sentences
  |> List.map (\(fs, t) ->
        List.foldr
          (\f (r, s) -> (String.join " " f ++ s ++ r, ", "))
          ("", t)
          fs
        |> Tuple.first
      )
  |> String.join " "
