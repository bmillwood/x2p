module Main exposing (main)

import Set
import String
import Tuple

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

import Corpus
import Parse
import Python
import Sentence

type alias Model =
  { sentences : List Sentence.Sentence }

type Msg
  = Input String

init : () -> (Model, Cmd Msg)
init () =
  ( { sentences = Sentence.tokenize Corpus.testCases }
  , Cmd.none
  )

fragsView : Sentence.Terminator -> List Sentence.Fragment -> Html a
fragsView terminator frags =
  List.foldr
    (\frag (part, term) -> (String.join " " frag ++ term ++ part, ", "))
    ("", terminator)
    frags
  |> Tuple.first
  |> Html.text

sentenceRow : Sentence.Sentence -> Html a
sentenceRow sentence =
  let
      (fragments, terminator) = sentence
      results = Parse.parse fragments
      resultView { interesting, thenBoring } =
        Html.span 
          []
          [ Html.pre [] [Html.text (Python.code interesting)]
          , fragsView terminator thenBoring
          ]
      isBoring = List.isEmpty results.results
  in
  Html.tr []
    [ Html.td
        (if isBoring
        then [Html.Attributes.style "color" "gray"]
        else [])
        [Html.text (Sentence.untokenize [sentence])]
    , Html.td [] (
        if isBoring
        then []
        else
          (fragsView "" results.before
            :: List.map resultView results.results)
        )
    ]

view : Model -> Html Msg
view { sentences } =
  Html.div
    [ Html.Attributes.style "background-color" "black"
    , Html.Attributes.style "color" "white"
    ]
    [ Html.form []
        [ Html.textarea
            [ Html.Events.onInput Input
            , Html.Attributes.style "background-color" "black"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "width" "60em"
            , Html.Attributes.style "height" "10em"
            ]
            [ Html.text (Sentence.untokenize sentences)
            ]
        ]
    , Html.table [] (List.map sentenceRow sentences)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update (Input input) _ =
  ( { sentences = Sentence.tokenize input }
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
