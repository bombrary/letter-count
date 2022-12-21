module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias CountSpec =
    { limit: Int
    , countSpace: Bool
    , countNewline: Bool
    }


removeSpace : CountSpec -> String -> String
removeSpace spec text =
    if spec.countSpace then
        text
    else
        text
        |> String.replace " " ""
        |> String.replace "　" ""


removeNewline : CountSpec -> String -> String
removeNewline spec text =
    if spec.countNewline then
        text
    else
        text
        |> String.replace "\n" ""


count : CountSpec -> String -> Int
count spec text =
    text
    |> removeSpace spec
    |> removeNewline spec
    |> String.length


main = Browser.sandbox 
  { init = init
  , update = update
  , view = view
  }


type alias Model =
  { textarea: String
  , countSpec: CountSpec
  }


init : Model
init = 
  { textarea = ""
  , countSpec = CountSpec 400 True True
  }


type Msg
  = Changed String
  | ClickedClear
  | ChangedLimit String
  | ToggleNewLine
  | ToggleSpace


update : Msg -> Model -> Model
update msg model =
  case msg of
    Changed textarea -> { model | textarea = textarea }

    ClickedClear -> { model | textarea = "" }

    ToggleNewLine -> 
        let countSpec = model.countSpec
            newCountSpec =
                { countSpec | countNewline = Basics.not countSpec.countNewline }
        in
        { model | countSpec = newCountSpec }

    ToggleSpace -> 
        let countSpec = model.countSpec
            newCountSpec =
                { countSpec | countSpace = Basics.not countSpec.countSpace }
        in
        { model | countSpec = newCountSpec }

    ChangedLimit str ->
        case String.toInt str of
            Just limit ->
                let countSpec = model.countSpec
                    newCountSpec =
                        { countSpec | limit = limit }
                in
                { model | countSpec = newCountSpec }

            Nothing ->
                model


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
    [ textarea [ onInput Changed, value model.textarea ] [ ]
    , counter model
    , div [ class "count-option" ]
      [ div
          [ class "count-option-newline"
          , onClick ToggleNewLine
          ]
          [ div
              [ class "check-mark" ]
              [ checkMark model.countSpec.countNewline ]
          , text "改行含む"
          ]
      , div
          [ class "count-option-space"
          , onClick ToggleSpace
          ]
          [ div
              [ class "check-mark" ]
              [ checkMark model.countSpec.countSpace ]
          , text "空白含む"
          ]
      ]
    , button [ class "clear-button", onClick ClickedClear ] [ text "Clear" ]
    ]


checkMark pred =
    if pred
        then text "✓"
        else text ""

           


counter : Model -> Html Msg
counter model =
    let len = count model.countSpec model.textarea
    in
      div
        [ colorRedGT model.countSpec.limit len
        , class "display-count"
        ]
        [ div []
            [ text ("現在の文字数"
                     ++ ": "
                     ++ (String.fromInt len)
                     ++ "/")
            ]
        , input
            [ class "count-limit"
            , onInput ChangedLimit
            , type_ "input"
            , value (String.fromInt model.countSpec.limit)
            ]
            []
        ]


colorRedGT : Int -> Int -> (Attribute Msg)
colorRedGT limit len =
  if len > limit then
    style "color" "#C2185B"
  else
    style "color" "inherit"
