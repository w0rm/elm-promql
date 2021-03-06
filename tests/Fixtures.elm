module Fixtures exposing (fail, succeed)

-- https://github.com/prometheus/prometheus/blob/master/promql/parse_test.go


succeed : List String
succeed =
    [ -- Scalars and scalar-to-scalar operations.
      "1"
    , "+Inf"
    , "-Inf"
    , ".5"
    , "5."
    , "123.4567"
    , "5e-3"
    , "5e3"
    , "0xc"
    , "0755"
    , "+5.5e-3"
    , "-0755"
    , "1 + 1"
    , "1 - 1"
    , "1 * 1"
    , "1 % 1"
    , "1 / 1"
    , "1 == bool 1"
    , "1 != bool 1"
    , "1 > bool 1"
    , "1 >= bool 1"
    , "1 < bool 1"
    , "1 <= bool 1"
    , "+1 + -2 * 1"
    , "1 + 2/(3*1)"
    , "1 < bool 2 - 1 * 2"
    , "-some_metric"
    , "+some_metric"

    -- Vector binary operations
    , "foo * bar"
    , "foo == 1"
    , "foo == bool 1"
    , "2.5 / bar"
    , "foo and bar"
    , "foo or bar"
    , "foo unless bar"
    , "foo + bar or bla and blub"
    , "foo and bar unless baz or qux"
    , "bar + on(foo) bla / on(baz, buz) group_right(test) blub"
    , "foo * on(test,blub) bar"
    , "foo * on(test,blub) group_left bar"
    , "foo and on(test,blub) bar"
    , "foo and on() bar"
    , "foo and ignoring(test,blub) bar"
    , "foo and ignoring() bar"
    , "foo unless on(bar) baz"
    , "foo / on(test,blub) group_left(bar) bar"
    , "foo / ignoring(test,blub) group_left(blub) bar"
    , "foo / ignoring(test,blub) group_left(bar) bar"
    , "foo - on(test,blub) group_right(bar,foo) bar"
    , "foo - ignoring(test,blub) group_right(bar,foo) bar"

    -- Test Vector selector
    , "foo"
    , "foo offset 5m"
    , "foo:bar{a=\"bc\"}"
    , "foo{NaN='bc'}"
    , "foo{a=\"b\", foo!=\"bar\", test=~\"test\", bar!~\"baz\"}"

    -- Test matrix selector
    , "test[5s]"
    , "test[5m]"
    , "test[5h] OFFSET 5m"
    , "test[5d] OFFSET 10s"
    , "test[5w] offset 2w"
    , "test{a=\"b\"}[5y] OFFSET 3d"

    -- Test aggregation
    , "sum by (foo)(some_metric)"
    , "avg by (foo)(some_metric)"
    , "max by (foo)(some_metric)"
    , "sum without (foo) (some_metric)"
    , "sum (some_metric) without (foo)"
    , "stddev(some_metric)"
    , "stdvar by (foo)(some_metric)"
    , "sum by ()(some_metric)"
    , "topk(5, some_metric)"
    , "count_values(\"value\", some_metric)"
    , "sum without(and, by, avg, count, alert, annotations)(some_metric)"

    -- Test function calls
    , "time()"
    , "floor(some_metric{foo!=\"bar\"})"
    , "rate(some_metric[5m])"
    , "round(some_metric)"
    , "round(some_metric, 5)"

    -- String quoting and escape sequence interpretation tests
    , "\"double-quoted string \\\" with escaped quote\""
    , "'single-quoted string \\' with escaped quote'"
    , "`backtick-quoted string`"
    , "\"\\a\\b\\f\\n\\r\\t\\v\\\" - \\xFF\\377\\u1234\\U00010111\\U0001011111☺\""
    , "'\\a\\b\\f\\n\\r\\t\\v\\' - \\xFF\\377\\u1234\\U00010111\\U0001011111☺'"
    , "`\\a\\b\\f\\n\\r\\t\\v\\\"\\' - \\xFF\\377\\u1234\\U00010111\\U0001011111☺`"
    ]


fail : List String
fail =
    [ -- Scalars and scalar-to-scalar operations
      ""
    , "# just a comment\n\n"
    , "1+"
    , "."
    , "2.5."
    , "100..4"
    , "0deadbeef"
    , "1 /"
    , "*1"
    , "(1))"

    --, "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
    , "((1)"
    , "("
    , "1 and 1"
    , "1 == 1"
    , "1 or 1"
    , "1 unless 1"
    , "1 !~ 1"
    , "1 =~ 1"
    , "*test"
    , "1 offset 1d"
    , "a - on(b) ignoring(c) d"

    -- Vector binary operations
    , "foo and 1"
    , "1 and foo"
    , "foo or 1"
    , "1 or foo"
    , "foo unless 1"
    , "1 unless foo"
    , "1 or on(bar) foo"
    , "foo == on(bar) 10"
    , "foo and on(bar) group_left(baz) bar"
    , "foo and on(bar) group_right(baz) bar"
    , "foo or on(bar) group_left(baz) bar"
    , "foo or on(bar) group_right(baz) bar"
    , "foo unless on(bar) group_left(baz) bar"
    , "foo unless on(bar) group_right(baz) bar"
    , "http_requests{group=\"production\"} + on(instance) group_left(job,instance) cpu_count{type=\"smp\"}"
    , "foo + bool bar"
    , "foo + bool 10"
    , "foo and bool 10"

    -- Test Vector selector
    , "{"
    , "}"
    , "some{"
    , "some}"
    , "some_metric{a=b}"
    , "some_metric{a:b=\"b\"}"
    , "foo{a*\"b\"}"
    , "foo{a>=\"b\"}"
    , "some_metric{a=\"ÿ\"}"
    , "foo{gibberish}"
    , "foo{1}"
    , "{}"
    , "{x=\"\"}"
    , "{x=~\".*\"}"
    , "{x!~\".+\"}"
    , "{x!=\"a\"}"
    , "foo{__name__=\"bar\"}"

    -- Test matrix selector
    , "foo[5mm]"
    , "foo[0m]"
    , "foo[5m30s]"
    , "foo[5m] OFFSET 1h30m"
    , "foo[\"5m\"]"
    , "foo[]"
    , "foo[1]"
    , "some_metric[5m] OFFSET 1"
    , "some_metric[5m] OFFSET 1mm"
    , "some_metric[5m] OFFSET"
    , "some_metric OFFSET 1m[5m]"
    , "(foo + bar)[5m]"

    -- Test aggregation
    , "sum without(==)(some_metric)"
    , "sum some_metric by (test)"
    , "sum (some_metric) by test"
    , "sum () by (test)"
    , "MIN keep_common (some_metric)"
    , "MIN (some_metric) keep_common"
    , "sum (some_metric) without (test) by (test)"
    , "sum without (test) (some_metric) by (test)"
    , "topk(some_metric)"
    , "topk(some_metric, other_metric)"
    , "count_values(5, other_metric)"

    -- Test function calls
    , "floor()"
    , "floor(some_metric, other_metric)"
    , "floor(1)"
    , "non_existent_function_far_bar()"
    , "rate(some_metric)"
    , "label_replace(a, `b`, `cÿ`, `d`, `.*`)"

    -- Fuzzing regression tests
    , "-="
    , "++-++-+-+-<"
    , "e-+=/(0)"
    , "-If"

    -- String quoting and escape sequence interpretation tests
    , "`\\``"
    , "\"\\"
    , "\"\\c\""
    , "\"\\x.\""
    ]
