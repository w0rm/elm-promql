module Print exposing (..)

import AST exposing (..)


expression : Expression -> String
expression value =
    case value of
        UnaryExpr op expr ->
            operator op ++ expression expr

        AggregateExpr { op, param, expr, without, grouping } ->
            aggregator op
                ++ "("
                ++ Maybe.withDefault "" (Maybe.map (\p -> expression p ++ ", ") param)
                ++ expression expr
                ++ if grouping == [] then
                    ""
                   else if without then
                    " without (" ++ String.join ", " grouping ++ ")"
                   else
                    " by (" ++ String.join ", " grouping ++ ")"

        BinaryExpr lhs op rhs ->
            expression lhs ++ " " ++ operator op ++ " " ++ expression rhs

        FunctionCall { func, args } ->
            "FunctionCall"

        MatrixSelector { name, range, offset, labelMatchers } ->
            ""

        NumberLiteral string ->
            string

        ParenExpr expr ->
            "(" ++ expression expr ++ ")"

        StringLiteral string ->
            string

        VectorSelector { name, offset, labelMatchers } ->
            name
                ++ matchers labelMatchers
                ++ maybeOffset offset


maybeOffset : Maybe Duration -> String
maybeOffset offset =
    offset
        |> Maybe.map (\d -> " offset " ++ duration d)
        |> Maybe.withDefault ""


matchers : List Matcher -> String
matchers labelMatchers =
    if labelMatchers == [] then
        ""
    else
        "{"
            ++ (labelMatchers
                    |> List.map
                        (\{ name, op, value } ->
                            name ++ matchOperator op ++ value
                        )
                    |> String.join ", "
               )
            ++ "}"


duration : Duration -> String
duration { value, units } =
    value ++ units


operator : Operator -> String
operator op =
    case op of
        SUB ->
            "-"

        ADD ->
            "+"

        MUL ->
            "*"

        MOD ->
            "%"

        DIV ->
            "/"

        LAND ->
            "and"

        LOR ->
            "or"

        LUnless ->
            "unless"

        EQL ->
            "=="

        NEQ ->
            "!="

        LTE ->
            "<="

        LSS ->
            "<"

        GTE ->
            ">="

        GTR ->
            ">"

        POW ->
            "^"


matchOperator : MatchOperator -> String
matchOperator op =
    case op of
        MatchEqual ->
            "="

        MatchNotEqual ->
            "!="

        MatchRegexp ->
            "=~"

        MatchNotRegexp ->
            "!~"


aggregator : Aggregator -> String
aggregator agg =
    case agg of
        Avg ->
            "avg"

        Count ->
            "count"

        Sum ->
            "sum"

        Min ->
            "min"

        Max ->
            "max"

        Stddev ->
            "stddev"

        Stdvar ->
            "stdvar"

        TopK ->
            "topk"

        BottomK ->
            "bottomk"

        CountValues ->
            "count_values"

        Quantile ->
            "quantile"
