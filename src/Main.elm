module Main exposing (main)

import Browser
import Html exposing (Html, Attribute)
import Html.Attributes exposing (value, style, spellcheck)
import Html.Events exposing (onInput, onClick)
import Parse
import Highlight
import Parser
import Fixtures


results : List ( String, Bool )
results =
    Fixtures.succeed
        |> List.map
            (\input ->
                case Parser.run Parse.query (String.toLower input) of
                    Ok _ ->
                        ( input, True )

                    Err _ ->
                        ( input, False )
            )


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
            , Html.div []
                (results
                    |> List.map
                        (\( str, r ) ->
                            Html.p
                                [ style "color"
                                    (if r then
                                        "lightgreen"
                                     else
                                        "red"
                                    )
                                , onClick str
                                ]
                                [ Html.text str ]
                        )
                )
            ]


highghtedResult : String -> List (Html String) -> Html String
highghtedResult input result =
    let
        styles =
            [ style "width" "500px"
            , style "height" "200px"
            , style "border" "1px solid grey"
            , style "font" "14px/1 monospace"
            , style "padding" "5px"
            , style "position" "absolute"
            , style "background" "transparent"
            ]
    in
        Html.div [ style "width" "500px", style "height" "200px" ]
            [ Html.div styles result
            , Html.textarea
                ([ value input
                 , onInput identity
                 , spellcheck False
                 , style "color" "transparent"
                 , style "caret-color" "black"
                 ]
                    ++ styles
                )
                []
            ]
