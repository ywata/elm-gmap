module MainTests exposing (..)

import Expect
import Main exposing (exStartNode)
import Maybe exposing (withDefault)
import SharedModels exposing (..)
import Test exposing (..)


defaultPos : GMPos
defaultPos =
    { lat = 0.0
    , lng = 0.0
    }


tests : Test
tests =
    -- It's important to consider about rounding error.
    describe "Testing exStartNode function"
        [ test "with valid latitude and longitude" <|
            \_ ->
                exStartNode [ "12.0", "56.78", "extra" ]
                    |> Maybe.withDefault defaultPos
                    |> (.lat >> Expect.equal 12.0)
        , test "with missing latitude" <|
            \_ ->
                exStartNode [ "", "56.78", "extra" ]
                    |> Maybe.withDefault defaultPos
                    |> (.lng >> Expect.equal 0.0)
        , test "with missing longitude" <|
            \_ ->
                exStartNode [ "12.34", "", "extra" ]
                    |> Maybe.withDefault defaultPos
                    |> (.lat >> Expect.equal 0.0)
        , test "with empty list" <|
            \_ ->
                exStartNode []
                    |> Maybe.withDefault defaultPos
                    |> (.lng >> Expect.equal 0.0)
        ]
