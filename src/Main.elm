module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput, onClick)
import Parse
import Print
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
                    Print.expression input (Debug.log "result" expr)

                Err deadEnds ->
                    Debug.toString deadEnds
    in
        Html.div []
            [ Html.textarea [ value input, onInput identity ] []
            , Html.div [] [ Html.text result ]
            , Html.hr [] []
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
