module AST exposing (..)


type Expression
    = AggregateExpr
        -- aggregation operation on a Vector
        -- <aggr-op>([parameter,] <vector expression>) [without|by (<label list>)]
        { op : Aggregator
        , param : Maybe Expression -- parameter used by some aggregators
        , expr : Expression -- the Vector expression over which is aggregated
        , without : Bool -- whether to drop the given labels rather than keep them
        , grouping : List String -- the labels by which to group the Vector
        }
    | BinaryExpr
        -- operands on the respective sides of the operator
        Expression
        Operator
        Expression
    | FunctionCall
        { func : Func
        , args : List Expression
        }
    | MatrixSelector
        { name : String
        , range : Duration
        , offset : Duration
        , labelMatchers : List Matcher
        }
    | VectorSelector
        { name : String
        , labelMatchers : List Matcher
        , offset : Maybe Duration
        }
    | NumberLiteral String
    | ParenExpr
        -- wraps an expression so it cannot be disassembled
        -- as a consequence of operator precedence
        Expression
    | StringLiteral String
    | UnaryExpr
        -- an unary operation on another expression,
        -- only supported for scalars
        Operator
        Expression


type Operator
    = SUB -- "-"
    | ADD -- "+"
    | MUL -- "*"
    | MOD -- "%"
    | DIV -- "/"
    | LAND -- "and"
    | LOR -- "or"
    | LUnless -- "unless"
    | EQL -- "=="
    | NEQ -- "!="
    | LTE -- "<="
    | LSS -- "<"
    | GTE -- ">="
    | GTR -- ">"
      -- TODO: are these used?
      --| EQLRegex -- "=~"
      --| NEQRegex -- "!~"
    | POW -- "^"


type Aggregator
    = Avg -- "avg"
    | Count -- "count"
    | Sum -- "sum"
    | Min -- "min"
    | Max -- "max"
    | Stddev -- "stddev"
    | Stdvar -- "stdvar"
    | TopK -- "topk"
    | BottomK -- "bottomk"
    | CountValues -- "count_values"
    | Quantile -- "quantile"


type alias Duration =
    { value : String
    , units : String
    }


{-| Matcher models the matching of a label.
-}
type alias Matcher =
    { name : String
    , op : MatchOperator
    , value : String
    }


{-| MatchOperator is an enum for label matching types.
-}
type MatchOperator
    = MatchEqual -- ==
    | MatchNotEqual -- !=
    | MatchRegexp -- =~
    | MatchNotRegexp -- !~


{-| Supported functions
-}
type Func
    = Abs -- abs
    | Absent -- absent
    | AvgOverTime -- avg_over_time
    | Ceil -- ceil
    | Changes -- changes
    | ClampMax -- clamp_max
    | ClampMin -- clamp_min
    | CountOverTime -- count_over_time
    | DaysInMonth -- days_in_month
    | DayOfMonth -- day_of_month
    | DayOfWeek -- day_of_week
    | Delta -- delta
    | Deriv -- deriv
    | Exp -- exp
    | Floor -- floor
    | HistogramQuantile -- histogram_quantile
    | HoltWinters -- holt_winters
    | Hour -- hour
    | Idelta -- idelta
    | Increase -- increase
    | Irate -- irate
    | LabelReplace -- label_replace
    | LabelJoin -- label_join
    | Ln -- ln
    | Log10 -- log10
    | Log2 -- log2
    | MaxOverTime -- max_over_time
    | MinOverTime -- min_over_time
    | Minute -- minute
    | Month -- month
    | PredictLinear -- predict_linear
    | QuantileOverTime -- quantile_over_time
    | Rate -- rate
    | Resets -- resets
    | Round -- round
    | Scalar -- scalar
    | Sort -- sort
    | SortDesc -- sort_desc
    | Sqrt -- sqrt
    | StddevOverTime -- stddev_over_time
    | StdvarOverTime -- stdvar_over_time
    | SumOverTime -- sum_over_time
    | Time -- time
    | Timestamp -- timestamp
    | Vector -- vector
    | Year -- year
