module Main exposing (main)

import Set
import String
import Tuple

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

import Corpus
import Language
import Parse
import Sentence

type alias Model =
  { sentences : List Sentence.Sentence
  , debug : Bool
  , language  : Language.Language
  }

type Msg
  = Input String
  | SetDebug Bool
  | SetLanguage Language.Language

init : () -> (Model, Cmd Msg)
init () =
  ( { sentences = Sentence.tokenize Corpus.testCases
    , debug = False
    , language = Language.init
    }
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

sentenceRow
  : { debug : Bool }
 -> Language.Language
 -> Sentence.Sentence
 -> Html a
sentenceRow { debug } lang sentence =
  let
      (fragments, terminator) = sentence
      results = Parse.parse fragments
      resultView { interesting, thenBoring } =
        Html.span 
          []
          [ Html.pre [] [Html.text (Language.renderCode lang interesting)]
          , fragsView terminator thenBoring
          ]
      isBoring = List.isEmpty results.results
  in
  Html.tr [] (
      [ [ Html.td
          (if isBoring
          then [Html.Attributes.style "color" "gray"]
          else [])
          [Html.text (Sentence.untokenize [sentence])]
        ]
      , if debug
        then
          [ Html.text (Debug.toString (List.map .interesting results.results))
          ]
        else []
      , [ Html.td [] (
          if isBoring
          then []
          else
            (fragsView "" results.before
              :: List.map resultView results.results)
          )
        ]
      ] |> List.concat
    )

view : Model -> Html Msg
view { sentences, debug, language } =
  let
      radio lang =
        let
            name = Language.toString lang
        in
        [ Html.br [] []
        , Html.label []
            [ Html.input
                [ Html.Attributes.type_ "radio"
                , Html.Attributes.name "language"
                , Html.Attributes.value name
                , Html.Attributes.checked (language == lang)
                , Html.Events.onClick (SetLanguage lang)
                ]
                []
            , Html.text name
            ]
        ]
      debugCheckbox =
        [ Html.br [] []
        , Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.name "debug"
                , Html.Attributes.value "debug"
                , Html.Attributes.checked debug
                , Html.Events.onCheck SetDebug
                ]
                []
            , Html.text "Debug mode"
            ]
        ]
  in
  Html.div
    [ Html.Attributes.style "background-color" "black"
    , Html.Attributes.style "color" "white"
    ]
    [ Html.form [] (
          Html.textarea
            [ Html.Events.onInput Input
            , Html.Attributes.style "background-color" "black"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "width" "60em"
            , Html.Attributes.style "height" "10em"
            ]
            [ Html.text (Sentence.untokenize sentences)
            ]
            :: List.concatMap radio Language.all
            ++ debugCheckbox
        )
    , Html.table []
        (List.map (sentenceRow { debug = debug } language) sentences)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
      newModel =
        case msg of
          Input input ->
            { model | sentences = Sentence.tokenize input }
          SetDebug debug ->
            { model | debug = debug }
          SetLanguage language ->
            { model | language = language }
  in
  (newModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
