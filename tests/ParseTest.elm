module ParseTest exposing (fail, succeed)

import Expect exposing (Expectation)
import Fixtures
import Parse
import Parser
import Test exposing (Test)


succeed : Test
succeed =
    Fixtures.succeed
        |> List.map
            (\input ->
                input
                    |> String.toLower
                    |> Parser.run Parse.query
                    |> Expect.ok
                    |> always
                    |> Test.test input
            )
        |> Test.describe "Succeed:"


fail : Test
fail =
    Fixtures.fail
        |> List.map
            (\input ->
                input
                    |> String.toLower
                    |> Parser.run Parse.query
                    |> Expect.err
                    |> always
                    |> Test.test input
            )
        |> Test.describe "Fail:"
