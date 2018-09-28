module Parse exposing (query)

import AST exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


query : Parser Expression
query =
    Parser.succeed identity
        |. Parser.spaces
        |= expression
        |. Parser.end


expression : Parser Expression
expression =
    Parser.succeed identity
        |= Parser.oneOf
            [ binaryExpression
            , unaryExpression
            , primaryExpression
            ]
        |. Parser.spaces


unaryExpression : Parser Expression
unaryExpression =
    Parser.succeed UnaryExpr
        |= getOffset (Parser.oneOf (List.map Parser.symbol unaryOperators))
        |= Parser.lazy (\_ -> Parser.oneOf [ unaryExpression, primaryExpression ])


parenExpression : Parser Expression
parenExpression =
    Parser.succeed ParenExpr
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> expression)
        |. Parser.symbol ")"


primaryExpression : Parser Expression
primaryExpression =
    Parser.succeed identity
        |= Parser.oneOf
            [ parenExpression
            , numberLiteral
            , stringLiteral
            , aggregateExpression
            , functionCall
            , selectorWithoutName
            , selector
            ]
        |. Parser.spaces


aggregateExpression : Parser Expression
aggregateExpression =
    Parser.succeed
        (\op aggregateExpr -> aggregateExpr op)
        |= getOffset (Parser.oneOf (List.map Parser.keyword aggregateOperators))
        |. Parser.spaces
        |= Parser.oneOf
            [ aggregateExpression1 -- group followed by arguments
            , aggregateExpression2 -- arguments followed by group
            ]


aggregateExpression1 : Parser (Offset -> Expression)
aggregateExpression1 =
    Parser.succeed
        (\grp args off ->
            AggregateExpr1
                { operator = off
                , group = grp
                , arguments = args
                }
        )
        |= aggregateGroup
        |. Parser.spaces
        |= arguments


aggregateExpression2 : Parser (Offset -> Expression)
aggregateExpression2 =
    Parser.succeed
        (\args maybeGrp off ->
            AggregateExpr2
                { operator = off
                , arguments = args
                , maybeGroup = maybeGrp
                }
        )
        |= arguments
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.map Just aggregateGroup
            , Parser.succeed Nothing
            ]


functionCall : Parser Expression
functionCall =
    Parser.succeed
        (\func args -> FunctionCall { function = func, arguments = args })
        |= Parser.backtrackable
            (getOffset (Parser.oneOf (List.map Parser.keyword functions)))
        |. Parser.spaces
        |= arguments
        |. Parser.spaces


aggregateGroup : Parser AggregateGroup
aggregateGroup =
    Parser.succeed AggregateGroup
        |= getOffset (Parser.oneOf (List.map Parser.keyword groupingKeywords))
        |. Parser.spaces
        |= labels


binaryExpression : Parser Expression
binaryExpression =
    Parser.succeed
        (\leftExpr op mods rightExp ->
            BinaryExpr
                { leftExpression = leftExpr
                , operator = op
                , modifiers = mods
                , rightExpression = rightExp
                }
        )
        |= Parser.lazy (\_ -> Parser.backtrackable <| Parser.oneOf [ unaryExpression, primaryExpression ])
        |= getOffset
            (Parser.oneOf
                (List.map Parser.symbol arithmeticOperators
                    ++ List.map Parser.symbol comparisonOperators
                    ++ List.map Parser.keyword keywordOperators
                )
            )
        |. Parser.spaces
        |= modifiers
        |. Parser.spaces
        |= Parser.lazy (\_ -> expression)
        |. Parser.spaces


selectorWithoutName : Parser Expression
selectorWithoutName =
    Parser.succeed
        (\labelMatchers off ->
            Selector
                { name = Nothing
                , labelMatchers = labelMatchers
                , range = Nothing
                , offset = off
                }
        )
        |= matchers
        |. Parser.spaces
        |= offset


selector : Parser Expression
selector =
    Parser.succeed
        (\name labelMatchers rng offst ->
            Selector
                { name = Just name
                , labelMatchers = labelMatchers
                , range = rng
                , offset = offst
                }
        )
        |= getOffset
            (Parser.variable
                { start = isVarChar
                , inner = \c -> isVarChar c || c == ':'
                , reserved = keywords
                }
            )
        |. Parser.spaces
        |= Parser.oneOf [ matchers, Parser.succeed [] ]
        |. Parser.spaces
        |= range
        |. Parser.spaces
        |= offset


labels : Parser (List Offset)
labels =
    Parser.oneOf
        [ Parser.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = Parser.spaces
            , item = label
            , trailing = Parser.Forbidden
            }
        , Parser.succeed []
        ]


matchers : Parser (List Matcher)
matchers =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.spaces
        , item = matcher
        , trailing = Parser.Forbidden
        }


arguments : Parser (List Expression)
arguments =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> expression)
        , trailing = Parser.Forbidden
        }


range : Parser (Maybe Offset)
range =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "["
            |. Parser.spaces
            |= duration
            |. Parser.spaces
            |. Parser.symbol "]"
            |. Parser.spaces
        , Parser.succeed Nothing
        ]


offset : Parser (Maybe Offset)
offset =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.keyword offsetKeyword
            |. Parser.spaces
            |= duration
        , Parser.succeed Nothing
        ]


duration : Parser Offset
duration =
    getOffset <|
        Parser.succeed ()
            |. Parser.number
                { int = Just String.fromInt
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Nothing
                }
            |. Parser.oneOf (List.map Parser.token durationTokens)


modifiers : Parser (List Modifier)
modifiers =
    Parser.loop [] modifiersHelp


modifiersHelp : List Modifier -> Parser (Parser.Step (List Modifier) (List Modifier))
modifiersHelp revModifiers =
    -- TODO, only parse bool modifier for comparisonOperators?
    Parser.oneOf
        [ Parser.succeed (\mod -> Parser.Loop (mod :: revModifiers))
            |= modifier
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revModifiers))
        ]


modifier : Parser Modifier
modifier =
    Parser.oneOf
        [ Parser.succeed (\off -> Modifier off [])
            |= getOffset (Parser.keyword boolKeyword)
        , Parser.succeed Modifier
            |= getOffset (Parser.oneOf (List.map Parser.keyword groupingModifierKeywords))
            |. Parser.spaces
            |= labels
        ]


matcher : Parser Matcher
matcher =
    Parser.succeed Matcher
        |= getOffset label
        |. Parser.spaces
        |= getOffset (Parser.oneOf (List.map Parser.symbol matchOperators))
        |. Parser.spaces
        |= Parser.oneOf
            [ string '\''
            , string '"' -- "
            , string '`'
            ]


label : Parser Offset
label =
    getOffset <|
        Parser.variable
            { inner = \c -> isVarChar c || Char.isDigit c
            , start = isVarChar
            , reserved = Set.empty
            }


numberLiteral : Parser Expression
numberLiteral =
    Parser.succeed NumberLiteral
        |= number


number : Parser Offset
number =
    getOffset <|
        Parser.oneOf
            -- TODO: support octal: 0755
            (Parser.number
                { int = Just (always ())
                , hex = Just (always ())
                , octal = Nothing
                , binary = Nothing
                , float = Just (always ())
                }
                :: List.map Parser.keyword numberKeywords
            )


{-| TODO: should not allow escape
-}
stringLiteral : Parser Expression
stringLiteral =
    Parser.map StringLiteral <|
        Parser.oneOf
            [ string '\''
            , string '"' -- "
            , string '`'
            ]


string : Char -> Parser Offset
string separator =
    getOffset <|
        Parser.succeed ()
            |. Parser.token (String.fromChar separator)
            |. Parser.loop separator stringHelp


stringHelp : Char -> Parser (Parser.Step Char ())
stringHelp separator =
    Parser.oneOf
        [ Parser.succeed (Parser.Done ())
            |. Parser.token (String.fromChar separator)
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char == '\\')
            |. Parser.chompIf (\_ -> True)
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char /= '\\' && char /= separator)
        ]


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')


durationTokens : List String
durationTokens =
    [ "s", "m", "h", "d", "w", "y" ]


unaryOperators : List String
unaryOperators =
    [ "-", "+" ]


matchOperators : List String
matchOperators =
    [ "=~", "=", "!=", "!~" ]


arithmeticOperators : List String
arithmeticOperators =
    [ "^", "*", "%", "/", "-", "+" ]


comparisonOperators : List String
comparisonOperators =
    [ "==", "!=", "<=", "<", ">=", ">" ]


keywordOperators : List String
keywordOperators =
    [ "and", "unless", "or" ]


functions : List String
functions =
    [ "abs"
    , "absent"
    , "avg_over_time"
    , "ceil"
    , "changes"
    , "clamp_max"
    , "clamp_min"
    , "count_over_time"
    , "days_in_month"
    , "day_of_month"
    , "day_of_week"
    , "delta"
    , "deriv"
    , "exp"
    , "floor"
    , "histogram_quantile"
    , "holt_winters"
    , "hour"
    , "idelta"
    , "increase"
    , "irate"
    , "label_replace"
    , "label_join"
    , "ln"
    , "log10"
    , "log2"
    , "max_over_time"
    , "min_over_time"
    , "minute"
    , "month"
    , "predict_linear"
    , "quantile_over_time"
    , "rate"
    , "resets"
    , "round"
    , "scalar"
    , "sort"
    , "sort_desc"
    , "sqrt"
    , "stddev_over_time"
    , "stdvar_over_time"
    , "sum_over_time"
    , "time"
    , "timestamp"
    , "vector"
    , "year"
    ]


aggregateOperators : List String
aggregateOperators =
    [ "avg"
    , "count"
    , "sum"
    , "min"
    , "max"
    , "stddev"
    , "stdvar"
    , "topk"
    , "bottomk"
    , "count_values"
    , "quantile"
    ]


boolKeyword : String
boolKeyword =
    "bool"


offsetKeyword : String
offsetKeyword =
    "offset"


groupingKeywords : List String
groupingKeywords =
    [ "without", "by" ]


numberKeywords : List String
numberKeywords =
    [ "nan", "inf" ]


getOffset : Parser a -> Parser Offset
getOffset parser =
    Parser.succeed Offset
        |= Parser.getOffset
        |. parser
        |= Parser.getOffset


keywords : Set String
keywords =
    Set.fromList
        (keywordOperators
            ++ aggregateOperators
            ++ groupingKeywords
            ++ numberKeywords
            ++ groupingModifierKeywords
            ++ [ "alert"
               , "if"
               , "for"
               , "labels"
               , "annotations"
               , boolKeyword
               , offsetKeyword
               ]
        )


groupingModifierKeywords : List String
groupingModifierKeywords =
    [ "on"
    , "ignoring"
    , "group_left"
    , "group_right"
    ]
