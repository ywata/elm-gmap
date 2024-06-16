module Main exposing (GMData(..), RPSemantic(..), RPType(..), evalEdgeProp, exStartNode)

import Browser
import Debug
import Dict exposing (Dict, update)
import GMaps exposing (hide, mapMoved, moveMap, registerNode, registerPath, show)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes as Attr exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (getAt)
import SharedModels exposing (GMPos)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias EdgeProp =
    List String


type alias SegmentProps =
    List EdgeProp


type alias RoadProp =
    { key : String
    , vec : List SegmentProps
    }


type RPType
    = RPIBool
    | RPFloat


type RPSemantic
    = StartNode
    | EndNode
    | Edge
    | Quantity


type alias RPAttr =
    { name : String
    , rpType : RPType
    , semantic : RPSemantic
    }


type alias Model =
    { pos : GMPos
    , path : List GMPos
    , roadProps : List RoadProp
    , rpAttr : Dict Int RPAttr
    , rpAttrState : Dict Int Bool
    }


type GMData a
    = Point a
    | PolyLine (List a)


roadPropDecoder : Decoder RoadProp
roadPropDecoder =
    Decode.map2
        RoadProp
        (Decode.field "key" Decode.string)
        (Decode.field "vec" (Decode.list (Decode.list (Decode.list Decode.string))))


getRoadProp : Cmd Msg
getRoadProp =
    Http.get
        { url = "/test-data.json"
        , expect = Http.expectJson GotRoadProp (Decode.list roadPropDecoder)
        }



-- Helper functions


iboolToBool : String -> Maybe Bool
iboolToBool s =
    case s of
        "0" ->
            Just False

        "1" ->
            Just True

        _ ->
            Nothing


exStartNode : EdgeProp -> Maybe GMPos
exStartNode vs =
    case vs of
        lat :: lng :: _ ->
            Maybe.map2 GMPos (String.toFloat lat) (String.toFloat lng)

        _ ->
            Nothing


exEndNode : EdgeProp -> Maybe GMPos
exEndNode vs =
    case vs of
        _ :: _ :: rest ->
            exStartNode rest

        _ ->
            Nothing


exEdge : EdgeProp -> Maybe (List GMPos)
exEdge vs =
    let
        s =
            exStartNode vs

        e =
            exEndNode vs
    in
    Maybe.map2 (\x y -> [ x, y ]) s e


isJust : Maybe a -> Bool
isJust rs =
    case rs of
        Just _ ->
            True

        _ ->
            False


classifyByKey : List ( comparable, a ) -> Dict comparable (List a)
classifyByKey list =
    List.foldl addKeyValuePair Dict.empty list


addKeyValuePair : ( comparable, v ) -> Dict comparable (List v) -> Dict comparable (List v)
addKeyValuePair ( k, v ) dict =
    let
        updateFunction maybeList =
            case maybeList of
                Just oldList ->
                    Just (oldList ++ [ v ])

                Nothing ->
                    Just [ v ]
    in
    Dict.update k updateFunction dict


evalEdgeProp : Dict Int RPAttr -> EdgeProp -> Dict Int (List (GMData GMPos))
evalEdgeProp rpaDic ep =
    let
        epp =
            List.indexedMap Tuple.pair ep
                |> List.map
                    (\( i, _ ) ->
                        Dict.get i rpaDic
                            |> Maybe.map (\val -> ( i, val, ep ))
                    )
                |> List.filter isJust
                |> List.map
                    (Maybe.withDefault
                        ( -1, { name = "", rpType = RPFloat, semantic = Edge }, [] )
                    )
                |> List.map
                    (\( i, attr, epx ) ->
                        let
                            ibool =
                                List.Extra.getAt i epx |> Maybe.andThen iboolToBool
                        in
                        case ( attr.rpType, attr.semantic, ibool ) of
                            ( RPIBool, Edge, Just True ) ->
                                exEdge ep
                                    |> Maybe.map (\v -> ( i, PolyLine v ))

                            ( RPIBool, StartNode, Just True ) ->
                                exStartNode ep
                                    |> Maybe.map (\v -> ( i, Point v ))

                            --Just ( i, exStartNode ep |> Maybe.map Marker )
                            ( RPIBool, EndNode, Just True ) ->
                                exEndNode ep
                                    |> Maybe.map (\v -> ( i, Point v ))

                            _ ->
                                Nothing
                    )
                |> List.filter isJust
                |> List.map (Maybe.withDefault ( -1, Point { lat = 0.0, lng = 0.0 } ))
    in
    classifyByKey epp


isPoint : GMData a -> Bool
isPoint g =
    case g of
        Point _ ->
            True

        _ ->
            False


isPolyLine : GMData a -> Bool
isPolyLine g =
    case g of
        PolyLine _ ->
            True

        _ ->
            False


nullify : Maybe ( a, List b ) -> Maybe ( a, List b )
nullify mb =
    mb
        |> Maybe.andThen
            (\v ->
                case v of
                    ( _, [] ) ->
                        Nothing

                    otherwise ->
                        Just otherwise
            )


getPointsByIndex : Dict Int (List (GMData GMPos)) -> Int -> Maybe ( Int, List GMPos )
getPointsByIndex d i =
    Dict.get i d
        |> Maybe.map
            (\v ->
                v
                    |> List.filter isPoint
                    |> List.map
                        (\g ->
                            case g of
                                Point a ->
                                    a

                                _ ->
                                    { lat = 0.0, lng = 0.0 }
                        )
            )
        |> Maybe.map (\v -> ( i, v ))
        |> nullify


getPolyLinesByIndex : Dict Int (List (GMData GMPos)) -> Int -> Maybe ( Int, List (List GMPos) )
getPolyLinesByIndex d i =
    Dict.get i (Debug.log "getPolyLinesByIndex" d)
        |> Maybe.map
            (\v ->
                v
                    |> List.filter isPolyLine
                    |> List.map
                        (\g ->
                            case g of
                                PolyLine a ->
                                    a

                                _ ->
                                    [ { lat = 0.0, lng = 0.0 } ]
                        )
            )
        |> Maybe.map (\v -> ( i, v ))
        |> nullify



-- UPDATE


type Msg
    = MapMoved GMPos
    | Load
    | GotRoadProp (Result Http.Error (List RoadProp))
    | ToggleRpAttrState Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMoved newPos ->
            ( { model | pos = newPos }
            , Cmd.none
            )

        Load ->
            ( model, getRoadProp )

        GotRoadProp (Ok rps) ->
            let
                all =
                    rps
                        |> List.map .vec
                        |> List.map (List.map (List.map (evalEdgeProp model.rpAttr)))
                        |> List.concat
                        |> List.concat
                        |> List.map Dict.toList
                        |> List.concat
                        |> classifyByKey
                        |> Dict.map (\_ v -> List.concat v)

                keys =
                    Dict.keys all

                points =
                    keys
                        |> List.map (getPointsByIndex all)
                        |> List.filter isJust
                        |> List.map (Maybe.withDefault ( -1, [] ))

                polylines =
                    keys
                        |> List.map (getPolyLinesByIndex all)
                        |> List.filter isJust
                        |> List.map (Maybe.withDefault ( -1, [] ))

                pointCmds =
                    points |> List.map registerNode

                plCmds =
                    polylines |> List.map registerPath

                showCmds =
                    model.rpAttrState
                        |> Dict.toList
                        |> List.filter Tuple.second
                        |> List.map Tuple.first
                        |> List.map show

                cmds =
                    pointCmds ++ plCmds ++ showCmds
            in
            ( { model | roadProps = rps }
            , Cmd.batch cmds
            )

        GotRoadProp (Err err) ->
            let
                _ =
                    Debug.log "GotRoadProp:error" err
            in
            ( model
            , Cmd.none
            )

        ToggleRpAttrState id ->
            let
                currState =
                    Dict.get id model.rpAttrState

                cmd =
                    case currState of
                        Nothing ->
                            Cmd.none

                        Just True ->
                            hide id

                        Just False ->
                            show id
            in
            ( { model | rpAttrState = Dict.update id (Maybe.map not) model.rpAttrState }
            , cmd
            )



-- VIEW


viewRoadPropCheckBoxes : Model -> Html Msg
viewRoadPropCheckBoxes model =
    let
        checkBox id =
            let
                checked =
                    Dict.get id model.rpAttrState |> Maybe.withDefault False

                propName =
                    Dict.get id model.rpAttr |> Maybe.withDefault { name = "should never used", rpType = RPIBool, semantic = Edge } |> .name
            in
            div []
                [ input
                    [ Attr.type_ "checkbox"
                    , Attr.checked checked
                    , onClick <| ToggleRpAttrState id
                    ]
                    []
                , text (" " ++ propName)
                ]

        rpAttrIds =
            Dict.toList model.rpAttr
                |> List.filter (\( _, v ) -> v.rpType == RPIBool)
                |> List.map Tuple.first
    in
    Html.div [] <| List.map checkBox rpAttrIds


view : Model -> Html Msg
view model =
    case True of
        _ ->
            div []
                [ div []
                    [ button [ onClick Load ] [ text "Load" ]
                    , viewRoadPropCheckBoxes model
                    ]
                , div [ Attr.class "columns" ]
                    [ div [ Attr.id "sidebar", Attr.class "column is-one-fifth" ]
                        [ p [] [ text ("Latitude: " ++ String.fromFloat model.pos.lat) ]
                        , p [] [ text ("Longitude: " ++ String.fromFloat model.pos.lng) ]
                        ]
                    , div [ Attr.id "map", Attr.class "column" ] []
                    ]
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    mapMoved MapMoved



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        tokyo =
            GMPos 35.0 135.0

        attrs =
            [ { name = "slat"
              , rpType = RPFloat
              , semantic = Quantity
              }
            , { name = "slng"
              , rpType = RPFloat
              , semantic = Quantity
              }
            , { name = "elat"
              , rpType = RPFloat
              , semantic = Quantity
              }
            , { name = "elng"
              , rpType = RPFloat
              , semantic = Quantity
              }
            , { name = "dist"
              , rpType = RPFloat
              , semantic = Quantity
              }
            , { name = "prop1"
              , rpType = RPIBool
              , semantic = StartNode
              }
            , { name = "prop2"
              , rpType = RPIBool
              , semantic = Edge
              }
            , { name = "prop3"
              , rpType = RPIBool
              , semantic = EndNode
              }
            ]

        rpAttr =
            Dict.fromList (List.indexedMap Tuple.pair attrs)

        rpAttrState =
            Dict.keys rpAttr |> List.foldl (\k acc -> Dict.insert k False acc) Dict.empty
    in
    ( Model tokyo
        []
        []
        rpAttr
        rpAttrState
    , moveMap tokyo
    )
