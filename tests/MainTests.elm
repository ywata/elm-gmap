module MainTests exposing (..)

import Dict exposing (fromList)
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


test_exStartNode : Test
test_exStartNode =
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


test_evalEdgeProp : Test
test_evalEdgeProp =
    describe "Testing evalEdgeProp function"
        [ test "evalEdgeProp valid" <|
            \_ ->
                Main.evalEdgeProp [ "0", "1" ]
                    ([ { name = "prop1"
                       , rpType = Main.RPIBool
                       , semantic = Main.Edge
                       }
                     , { name = "prop1"
                       , rpType = Main.RPIBool
                       , semantic = Main.StartNode
                       }
                     ]
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                    )
                    |> Expect.equal [ Just False, Just True ]
        ]
