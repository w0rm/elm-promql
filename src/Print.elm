module Print exposing (..)

import AST exposing (..)
import String


expression : String -> Expression -> String
expression input value =
    case value of
        UnaryExpr op expr ->
            sliceOffset input op ++ expression input expr

        AggregateExpr { op, args, group } ->
            sliceOffset input op
                ++ "("
                ++ String.join ", " (List.map (expression input) args)
                ++ ")"
                ++ case group of
                    Nothing ->
                        ""

                    Just { grouping, labels } ->
                        " "
                            ++ sliceOffset input grouping
                            ++ " ("
                            ++ String.join ", " (List.map (sliceOffset input) labels)
                            ++ ")"

        BinaryExpr lhs op rhs ->
            expression input lhs
                ++ " "
                ++ sliceOffset input op.op
                ++ modifiers input op.modifiers
                ++ " "
                ++ expression input rhs

        FunctionCall { func, args } ->
            sliceOffset input func
                ++ "("
                ++ String.join ", " (List.map (expression input) args)
                ++ ")"

        NumberLiteral num ->
            sliceOffset input num

        ParenExpr expr ->
            "(" ++ expression input expr ++ ")"

        StringLiteral str ->
            sliceOffset input str

        Selector { name, range, offset, labelMatchers } ->
            (case name of
                Just n ->
                    " " ++ sliceOffset input n

                Nothing ->
                    ""
            )
                ++ matchers input labelMatchers
                ++ maybeRange input range
                ++ maybeOffset input offset


modifiers : String -> List Modifier -> String
modifiers input mds =
    case mds of
        [] ->
            ""

        mods ->
            " "
                ++ String.join " "
                    (List.map
                        (\mod ->
                            sliceOffset input mod.name
                                ++ (case mod.labels of
                                        [] ->
                                            ""

                                        lbls ->
                                            " ("
                                                ++ String.join ", " (List.map (sliceOffset input) lbls)
                                                ++ ")"
                                   )
                        )
                        mods
                    )


maybeOffset : String -> Maybe Offset -> String
maybeOffset input maybeDuration =
    maybeDuration
        |> Maybe.map (\off -> " offset " ++ sliceOffset input off)
        |> Maybe.withDefault ""


maybeRange : String -> Maybe Offset -> String
maybeRange input range =
    range
        |> Maybe.map (\off -> "[" ++ sliceOffset input off ++ "]")
        |> Maybe.withDefault ""


matchers : String -> List Matcher -> String
matchers input labelMatchers =
    if labelMatchers == [] then
        ""
    else
        "{"
            ++ (labelMatchers
                    |> List.map
                        (\{ name, op, value } ->
                            sliceOffset input name
                                ++ sliceOffset input op
                                ++ sliceOffset input value
                        )
                    |> String.join ", "
               )
            ++ "}"


sliceOffset : String -> Offset -> String
sliceOffset input { start, end } =
    String.slice start end input
