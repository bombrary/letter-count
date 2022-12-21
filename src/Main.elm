module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Browser.sandbox 
  { init = init
  , update = update
  , view = view
  }
type alias Model =
  { textarea: String
  }

init : Model
init = 
  { textarea = ""
  }

type Msg
  = Changed String
  | ClickedClear

update : Msg -> Model -> Model
update msg model =
  case msg of
    Changed textarea -> { model | textarea = textarea }

    ClickedClear -> { model | textarea = "" }

view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
    [ textarea [ onInput Changed, value model.textarea ] [ ]
    , ul []
      [ liSSG model
      , li140 model
      , liOthers model
      ]
    , button [ class "clear-button", onClick ClickedClear ] [ text "Clear" ]
    ]

liSSG : Model -> Html Msg
liSSG model =
  let len = model.textarea
                |> String.length
  in
    li
    [ colorRedGT 400 len ]
    [ text
        ("SSG用(空白・改行を数える): "
        ++ (String.fromInt len)
        ++ "/400"
        )
    ]

li140 : Model -> Html Msg
li140 model =
  let len = model.textarea
                |> String.length
  in
    li
    [ colorRedGT 140 len ]
    [ text
        ("140字小説用(空白・改行を数える): "
        ++ (String.fromInt len)
        ++ "/140"
        )
    ]

liTokyoNP : Model -> Html Msg
liTokyoNP model =
  let len = model.textarea
                |> String.replace "\n" ""
                |> String.length
  in
    li
    [ colorRedGT 300 len ]
    [ text
        ("東京新聞用(改行のみ数えない): "
        ++ (String.fromInt len)
        ++ "/300"
        )
    ]

liOthers : Model -> Html Msg
liOthers model =
  let len = model.textarea
                |> String.replace " " ""
                |> String.replace "　" ""
                |> String.replace "\n" ""
                |> String.length
  in
    li
    []
    [ text
        ("その他(空白・改行を数えない):"
        ++ (String.fromInt len)
        )
    ]

colorRedGT : Int -> Int -> (Attribute Msg)
colorRedGT limit len =
  if len > limit then
    style "color" "#C2185B"
  else
    style "color" "inherit"
