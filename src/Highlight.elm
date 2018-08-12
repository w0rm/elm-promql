module Highlight exposing (..)

import Html exposing (span, text, Html, Attribute)
import Html.Attributes exposing (style)
import AST exposing (..)
import String


type Term
    = KeywordTerm
    | OperatorTerm
    | LabelTerm
    | StringTerm
    | NumberTerm
    | MetricTerm
    | FunctionTerm
    | OffsetTerm


color : Term -> Attribute a
color t =
    case t of
        KeywordTerm ->
            style "color" "darkgray"

        OperatorTerm ->
            style "color" "darkgray"

        StringTerm ->
            style "color" "green"

        NumberTerm ->
            style "color" "green"

        LabelTerm ->
            style "color" "blue"

        FunctionTerm ->
            style "color" "darkgray"

        MetricTerm ->
            style "color" "blue"

        OffsetTerm ->
            style "color" "green"


highlight : String -> Expression -> List (Html a)
highlight input expr =
    expression expr
        |> List.foldl
            (\( term, offset ) ( currOffset, terms ) ->
                if offset.start > currOffset then
                    ( offset.end
                    , span [ color term ] [ text (String.slice offset.start offset.end input) ]
                        :: text (String.slice currOffset offset.start input)
                        :: terms
                    )
                else
                    ( offset.end
                    , span [ color term ] [ text (String.slice offset.start offset.end input) ]
                        :: terms
                    )
            )
            ( 0, [] )
        |> (\( currOffset, terms ) ->
                text (String.slice currOffset (String.length input) input) :: terms
           )
        |> List.reverse


expression : Expression -> List ( Term, Offset )
expression value =
    case value of
        UnaryExpr op expr ->
            ( OperatorTerm, op ) :: expression expr

        AggregateExpr1 op { grouping, labels } args ->
            ( KeywordTerm, op )
                :: ( KeywordTerm, grouping )
                :: List.map (Tuple.pair LabelTerm) labels
                ++ List.concatMap expression args

        AggregateExpr2 op args group ->
            ( KeywordTerm, op )
                :: List.concatMap expression args
                ++ case group of
                    Nothing ->
                        []

                    Just { grouping, labels } ->
                        ( KeywordTerm, grouping )
                            :: List.map (Tuple.pair LabelTerm) labels

        BinaryExpr lhs op rhs ->
            expression lhs
                ++ [ ( OperatorTerm, op.op ) ]
                ++ List.concatMap modifier op.modifiers
                ++ expression rhs

        FunctionCall { func, args } ->
            ( FunctionTerm, func ) :: List.concatMap expression args

        NumberLiteral num ->
            [ ( NumberTerm, num ) ]

        ParenExpr expr ->
            expression expr

        StringLiteral str ->
            [ ( StringTerm, str ) ]

        Selector { name, range, offset, labelMatchers } ->
            (case name of
                Just n ->
                    [ ( MetricTerm, n ) ]

                Nothing ->
                    []
            )
                ++ List.concatMap matcher labelMatchers
                ++ maybeOffset range
                ++ maybeOffset offset


modifier : Modifier -> List ( Term, Offset )
modifier { name, labels } =
    ( KeywordTerm, name )
        :: List.map (Tuple.pair LabelTerm) labels


maybeOffset : Maybe Offset -> List ( Term, Offset )
maybeOffset offset =
    -- TODO: offset keyword
    case offset of
        Just off ->
            [ ( OffsetTerm, off ) ]

        Nothing ->
            []


matcher : Matcher -> List ( Term, Offset )
matcher { name, op, value } =
    [ ( LabelTerm, name ), ( OperatorTerm, op ), ( StringTerm, value ) ]


sliceOffset : String -> Int -> Int -> String
sliceOffset input start end =
    String.slice start end input
