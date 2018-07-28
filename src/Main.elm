module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Parse
import Print
import Parser


main : Program () String String
main =
    Browser.sandbox
        { init = ""
        , view = view
        , update = \msg _ -> msg
        }


view : String -> Html String
view input =
    let
        result =
            case Parser.run Parse.query input of
                Ok expr ->
                    Print.expression (Debug.log "result" expr)

                Err deadEnds ->
                    Debug.toString deadEnds
    in
        Html.div []
            [ Html.textarea [ value input, onInput identity ] []
            , Html.div [] [ Html.text result ]
            ]
