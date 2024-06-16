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
                Main.evalEdgeProp
                    ([ { name = "slat"
                       , rpType = Main.RPFloat
                       , semantic = Main.Quantity
                       }
                     , { name = "slng"
                       , rpType = Main.RPFloat
                       , semantic = Main.Quantity
                       }
                     , { name = "elat"
                       , rpType = Main.RPFloat
                       , semantic = Main.Quantity
                       }
                     , { name = "elng"
                       , rpType = Main.RPFloat
                       , semantic = Main.Quantity
                       }
                     , { name = "dist"
                       , rpType = Main.RPFloat
                       , semantic = Main.Quantity
                       }
                     , { name = "prop1"
                       , rpType = Main.RPIBool
                       , semantic = Main.StartNode
                       }
                     , { name = "prop2"
                       , rpType = Main.RPIBool
                       , semantic = Main.Edge
                       }
                     , { name = "prop3"
                       , rpType = Main.RPIBool
                       , semantic = Main.EndNode
                       }
                     ]
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                    )
                    [ "10.0", "11.0", "20.0", "21.0", "3.9", "0", "1", "1" ]
                    |> Expect.equal (Dict.fromList [ ( 6, [ Main.PolyLine [ { lat = 10, lng = 11 }, { lat = 20, lng = 21 } ] ] ), ( 7, [ Main.Point { lat = 20, lng = 21 } ] ) ])
        ]
