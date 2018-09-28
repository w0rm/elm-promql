module Highlight exposing (highlight)

import AST exposing (..)
import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (style)
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

        AggregateExpr1 { operator, group, arguments } ->
            ( KeywordTerm, operator )
                :: ( KeywordTerm, group.grouping )
                :: List.map (Tuple.pair LabelTerm) group.labels
                ++ List.concatMap expression arguments

        AggregateExpr2 { operator, arguments, maybeGroup } ->
            ( KeywordTerm, operator )
                :: List.concatMap expression arguments
                ++ (case maybeGroup of
                        Nothing ->
                            []

                        Just { grouping, labels } ->
                            ( KeywordTerm, grouping )
                                :: List.map (Tuple.pair LabelTerm) labels
                   )

        BinaryExpr { leftExpression, operator, modifiers, rightExpression } ->
            expression leftExpression
                ++ [ ( OperatorTerm, operator ) ]
                ++ List.concatMap modifier modifiers
                ++ expression rightExpression

        FunctionCall { function, arguments } ->
            ( FunctionTerm, function ) :: List.concatMap expression arguments

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
    [ ( LabelTerm, name )
    , ( OperatorTerm, op )
    , ( StringTerm, value )
    ]


sliceOffset : String -> Int -> Int -> String
sliceOffset input start end =
    String.slice start end input
