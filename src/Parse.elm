module Parse exposing (query)

import AST exposing (..)
import Parser exposing (Parser, (|=), (|.))
import String
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
        |= Parser.oneOf
            [ Parser.succeed SUB |. Parser.symbol "-"
            , Parser.succeed ADD |. Parser.symbol "+"
            ]
        |= Parser.lazy (\_ -> primaryExpression)


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
            , vectorSelectorWithoutName
            , vectorSelector
            ]
        |. Parser.spaces


binaryExpression : Parser Expression
binaryExpression =
    Parser.oneOf
        [ binary1
        , binary2
        , binary3
        , binary4
        , binary5
        , binary6
        ]


binary1 : Parser Expression
binary1 =
    operator operator1 operand1


operand1 : Parser Expression
operand1 =
    Parser.oneOf [ primaryExpression, unaryExpression ]


binary2 : Parser Expression
binary2 =
    operator operator2 operand2


operand2 : Parser Expression
operand2 =
    Parser.oneOf [ operand1, binary1 ]


binary3 : Parser Expression
binary3 =
    operator operator3 operand3


operand3 : Parser Expression
operand3 =
    Parser.oneOf [ operand2, binary2 ]


binary4 : Parser Expression
binary4 =
    operator operator4 operand4


operand4 : Parser Expression
operand4 =
    Parser.oneOf [ operand3, binary3 ]


binary5 : Parser Expression
binary5 =
    operator operator5 operand5


operand5 : Parser Expression
operand5 =
    Parser.oneOf [ operand4, binary4 ]


binary6 : Parser Expression
binary6 =
    operator operator6 operand6


operand6 : Parser Expression
operand6 =
    Parser.oneOf [ operand5, binary5 ]


operator : Parser Operator -> Parser Expression -> Parser Expression
operator op operand =
    Parser.succeed BinaryExpr
        |= Parser.lazy (\_ -> Parser.backtrackable operand)
        |= op
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.lazy (\_ -> binaryExpression)
            , operand
            ]
        |. Parser.spaces


{-| TODO: check for the `__name__` in matchers
-}
vectorSelectorWithoutName : Parser Expression
vectorSelectorWithoutName =
    Parser.succeed
        (\labelMatchers off ->
            VectorSelector
                { name = ""
                , labelMatchers = labelMatchers
                , offset = off
                }
        )
        |= matchers
        |. Parser.spaces
        |= offset


vectorSelector : Parser Expression
vectorSelector =
    Parser.succeed
        (\name labelMatchers off ->
            VectorSelector
                { name = name
                , labelMatchers = labelMatchers
                , offset = off
                }
        )
        |= Parser.variable
            { start = isVarChar
            , inner = (\c -> isVarChar c || c == ':')
            , reserved = Set.empty -- TODO: add keywords
            }
        |= matchers
        |. Parser.spaces
        |= offset


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


offset : Parser (Maybe Duration)
offset =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.keyword "offset"
            |. Parser.spaces
            |= duration
        , Parser.succeed Nothing
        ]


duration : Parser Duration
duration =
    Parser.succeed Duration
        |= Parser.number
            { int = Just String.fromInt
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Nothing
            }
        |= Parser.oneOf
            [ Parser.succeed "s" |. Parser.symbol "s"
            , Parser.succeed "m" |. Parser.symbol "m"
            , Parser.succeed "h" |. Parser.symbol "h"
            , Parser.succeed "d" |. Parser.symbol "d"
            , Parser.succeed "w" |. Parser.symbol "w"
            , Parser.succeed "y" |. Parser.symbol "y"
            ]


operator1 : Parser Operator
operator1 =
    Parser.succeed POW |. Parser.symbol "^"


operator2 : Parser Operator
operator2 =
    Parser.oneOf
        [ Parser.succeed MUL |. Parser.symbol "*"
        , Parser.succeed MOD |. Parser.symbol "%"
        , Parser.succeed DIV |. Parser.symbol "/"
        ]


operator3 : Parser Operator
operator3 =
    Parser.oneOf
        [ Parser.succeed SUB |. Parser.symbol "-"
        , Parser.succeed ADD |. Parser.symbol "+"
        ]


operator4 : Parser Operator
operator4 =
    Parser.oneOf
        [ Parser.succeed EQL |. Parser.symbol "=="
        , Parser.succeed NEQ |. Parser.symbol "!="
        , Parser.succeed LTE |. Parser.symbol "<="
        , Parser.succeed LSS |. Parser.symbol "<"
        , Parser.succeed GTE |. Parser.symbol ">="
        , Parser.succeed GTR |. Parser.symbol ">"
        ]


operator5 : Parser Operator
operator5 =
    Parser.oneOf
        [ Parser.succeed LAND |. Parser.symbol "and"
        , Parser.succeed LUnless |. Parser.symbol "unless"
        ]


operator6 : Parser Operator
operator6 =
    Parser.succeed LOR |. Parser.symbol "or"


matcher : Parser Matcher
matcher =
    Parser.succeed Matcher
        |= Parser.variable
            { inner = \c -> isVarChar c || Char.isDigit c
            , start = isVarChar
            , reserved = Set.empty
            }
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed MatchRegexp |. Parser.symbol "=~"
            , Parser.succeed MatchEqual |. Parser.symbol "="
            , Parser.succeed MatchNotEqual |. Parser.symbol "!="
            , Parser.succeed MatchNotRegexp |. Parser.symbol "!~"
            ]
        |. Parser.spaces
        |= Parser.oneOf [ string '\'', string '"', string '`' ]


numberLiteral : Parser Expression
numberLiteral =
    Parser.number
        { int = Just (String.fromInt >> NumberLiteral)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just (String.fromFloat >> NumberLiteral)
        }


{-| TODO: should not allow escape
-}
stringLiteral : Parser Expression
stringLiteral =
    Parser.map StringLiteral <|
        Parser.oneOf
            [ string '\''
            , string '"'
            , string '`'
            ]


string : Char -> Parser String
string separator =
    Parser.getChompedString <|
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
