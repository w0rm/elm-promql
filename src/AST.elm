module AST exposing (..)


type Expression
    = AggregateExpr
        -- aggregation operation on a Vector
        -- <aggr-op>([parameter,] <vector expression>) [without|by (<label list>)]
        { op : Offset
        , args : List Expression -- parameter used by some aggregators, the Vector expression over which is aggregated
        , group : Maybe AggregateGroup
        }
    | BinaryExpr Expression Operator Expression
    | FunctionCall
        { func : Offset
        , args : List Expression
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


type alias Operator =
    { op : Offset
    , modifiers : List Modifier
    }


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
