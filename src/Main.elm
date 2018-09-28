module Main exposing (main)

import Browser
import Highlight
import Html exposing (Attribute, Html)
import Html.Attributes exposing (spellcheck, style, value)
import Html.Events exposing (onClick, onInput)
import Parse
import Parser


main : Program () String String
main =
    Browser.sandbox
        { init = ""
        , view = view
        , update = always
        }


view : String -> Html String
view input =
    let
        result =
            case Parser.run Parse.query (String.toLower input) of
                Ok expr ->
                    Highlight.highlight input (Debug.log "result" expr)

                Err deadEnds ->
                    [ (\_ -> Html.span [ style "color" "red" ] [ Html.text input ]) (Debug.log "error" deadEnds) ]
    in
    Html.div []
        [ highghtedResult input result
        ]


highghtedResult : String -> List (Html String) -> Html String
highghtedResult input result =
    let
        styles =
            [ style "width" "100%"
            , style "font" "14px monospace"
            , style "padding" "5px"
            , style "margin" "0"
            , style "white-space" "pre"
            , style "background" "transparent"
            , style "box-sizing" "border-box"
            ]
    in
    Html.div [ style "width" "500px", style "min-height" "40px", style "position" "relative" ]
        [ Html.div (styles ++ [ style "border" "1px solid transparent" ]) (result ++ [ Html.br [] [] ])
        , Html.textarea
            ([ value input
             , onInput identity
             , spellcheck False
             , style "resize" "none"
             , style "height" "100%"
             , style "position" "absolute"
             , style "color" "transparent"
             , style "caret-color" "black"
             , style "top" "0"
             , style "bottom" "0"
             , style "border" "1px solid black"
             , style "overflow" "hidden"
             ]
                ++ styles
            )
            []
        ]
