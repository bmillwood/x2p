module Main exposing (main)

import Set
import String

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
  ( { sentences = Sentence.tokenize Corpus.clapYourHands }
  , Cmd.none
  )

wordView : Sentence.Word -> Html a
wordView word =
  let
      keywords = Set.fromList ["if", "while", "for", "unless"]
      fixedAttributes =
        [ Html.Attributes.style "border" "0.1em solid blue"
        ]
      dynamicAttributes =
        if Set.member (String.toLower word) keywords
        then [ Html.Attributes.style "background-color" "salmon" ]
        else []
  in
  Html.span
    (fixedAttributes ++ dynamicAttributes)
    [ Html.text word
    ]

fragView : Sentence.Fragment -> Html a
fragView words =
  Html.span
    [ Html.Attributes.style "border" "0.1em solid lightgreen"
    , Html.Attributes.style "margin" "0.2em"
    , Html.Attributes.style "padding" "0.2em"
    ]
    (List.map wordView words |> List.intersperse (Html.text " "))

sentenceView : Sentence.Sentence -> Html a
sentenceView sentence =
  let
      results = Parse.parse sentence
      resultView { interesting, thenBoring } =
        Html.span 
          []
          (Html.pre [] [Html.text (Python.code interesting)]
            :: List.map fragView thenBoring)
  in
  Html.div
    [ Html.Attributes.style "border" "0.1em solid red"
    , Html.Attributes.style "margin" "0.3em"
    , Html.Attributes.style "padding" "0.3em"
    ]
    (List.concat
      [ List.map fragView results.before
      , List.map resultView results.results
      ])

view : Model -> Html Msg
view { sentences } =
  Html.div
    [ Html.Attributes.style "max-width" "50%"
    ]
    [ Html.form []
        [ Html.textarea
            [ Html.Events.onInput Input
            ]
            []
        ]
    , List.map sentenceView sentences
      |> Html.div []
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
