module AST exposing
    ( AggregateGroup
    , Expression(..)
    , Matcher
    , Modifier
    , Offset
    )


type Expression
    = AggregateExpr1
        { operator : Offset
        , group : AggregateGroup
        , arguments : List Expression
        }
    | AggregateExpr2
        { operator : Offset
        , arguments : List Expression
        , maybeGroup : Maybe AggregateGroup
        }
    | BinaryExpr
        { leftExpression : Expression
        , operator : Offset
        , modifiers : List Modifier
        , rightExpression : Expression
        }
    | FunctionCall
        { function : Offset
        , arguments : List Expression
        }
    | Selector
        { name : Maybe Offset
        , labelMatchers : List Matcher
        , range : Maybe Offset
        , offset : Maybe Offset
        }
    | NumberLiteral Offset
    | ParenExpr Expression
    | StringLiteral Offset
    | UnaryExpr Offset Expression


type alias Modifier =
    { name : Offset
    , labels : List Offset
    }


type alias AggregateGroup =
    { grouping : Offset -- grouping keyword "without" or "by"
    , labels : List Offset -- the labels by which to group the Vector
    }


{-| Matcher models the matching of a label.
-}
type alias Matcher =
    { name : Offset
    , op : Offset
    , value : Offset
    }


{-| Keeps an offset into the original source
-}
type alias Offset =
    { start : Int
    , end : Int
    }
