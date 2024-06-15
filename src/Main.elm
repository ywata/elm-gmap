module Main exposing (..)

import Browser
import Debug
import Dict exposing (..)
import GMaps exposing (..)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes as Attr exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, float, int, map2)
import SharedModels exposing (GMPos)



-- MAIN


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
    { key : String, vec : List (List (List String)) }


type RPType
    = RPInt
    | RPIBool
    | RPFloat


type RPSemantic
    = StartNode
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


posDecoder : Decoder GMPos
posDecoder =
    map2 GMPos (field "lat" float) (field "lng" float)


getLoc : Cmd Msg
getLoc =
    Http.get
        { url = "/api"
        , expect = Http.expectJson GotLoc posDecoder
        }


findRoadPropAttr : String -> Dict Int RPAttr -> Maybe ( Int, RPAttr )
findRoadPropAttr name rpAttr =
    Dict.toList rpAttr
        |> List.filter (\( key, rp ) -> rp.name == name)
        |> List.head


mask : Dict Int RPAttr -> EdgeProp -> String -> Maybe a -> Maybe a
mask attrs vs name mb =
    findRoadPropAttr name attrs
        |> Maybe.andThen
            (\( _, rpAttr ) ->
                if rpAttr.rpType == RPIBool then
                    mb

                else
                    Nothing
            )



-- Helper functions


extract : List SegmentProps -> (EdgeProp -> a) -> List a
extract _ _ =
    []


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



-- UPDATE


type Msg
    = Move Direction
    | MapMoved GMPos
    | Load
    | GotLoc (Result Http.Error GMPos)
    | GotRoadProp (Result Http.Error (List RoadProp))
    | ToggleRpAttrState Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let
                newPos =
                    movePos model.pos direction
            in
            ( { model | pos = newPos, path = model.path ++ [ newPos ] }
            , moveMap newPos
            )

        MapMoved newPos ->
            ( { model | pos = newPos }
            , Cmd.none
            )

        Load ->
            ( model, getRoadProp )

        GotRoadProp (Ok rps) ->
            let
                all_edges =
                    rps
                        |> List.map .vec
                        |> List.map (List.map (List.map (\x -> ( x, exEdge x ))))
                        |> List.concat
                        |> List.concat
                        |> List.map Tuple.second
                        |> List.map (Maybe.withDefault [ { lat = 0.0, lng = 0.0 }, { lat = 0.0, lng = 0.0 } ])

                all_nodes =
                    rps
                        |> List.map .vec
                        |> List.map (List.map (List.map (\x -> ( x, exStartNode x ))))
                        |> List.concat
                        |> List.concat
                        |> List.map Tuple.second
                        |> List.map (Maybe.withDefault { lat = 0.0, lng = 0.0 })

                _ =
                    Debug.log "all_nodes" all_nodes
            in
            ( { model | roadProps = rps }
            , Cmd.batch
                [ registerNode ( 1, all_nodes )
                , registerPath ( 2, all_edges )
                , show 1
                , show 2
                ]
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
                _ =
                    Debug.log "ToggleRpAttrState" model.rpAttrState
            in
            ( { model | rpAttrState = Dict.update id (Maybe.map not) model.rpAttrState }
            , hide 1
            )

        otherwise ->
            let
                _ =
                    Debug.log "Otherwise" otherwise
            in
            ( model, Cmd.none )


type Direction
    = North
    | South
    | West
    | East


movePos : GMPos -> Direction -> GMPos
movePos pos direction =
    case direction of
        North ->
            { pos | lat = pos.lat + 1 }

        South ->
            { pos | lat = pos.lat - 1 }

        West ->
            { pos | lng = pos.lng - 1 }

        East ->
            { pos | lng = pos.lng + 1 }



-- VIEW


viewRoadPropCheckBoxes : Model -> Html Msg
viewRoadPropCheckBoxes model =
    let
        checkBox id =
            let
                checked =
                    Dict.get id model.rpAttrState |> Maybe.withDefault False

                propName =
                    Dict.get id model.rpAttr |> Maybe.withDefault { name = "xxxx", rpType = RPIBool, semantic = Edge } |> .name
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
                |> List.filter (\( k, v ) -> v.rpType == RPIBool)
                |> List.map Tuple.first
    in
    Html.div [] <| List.map checkBox rpAttrIds


view : Model -> Html Msg
view model =
    case True of
        _ ->
            div []
                [ div []
                    [ button [ onClick (Move North) ] [ text "North" ]
                    , button [ onClick (Move South) ] [ text "South" ]
                    , button [ onClick (Move West) ] [ text "West" ]
                    , button [ onClick (Move East) ] [ text "East" ]
                    , button [ onClick Load ] [ text "Load" ]
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
subscriptions model =
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
              , semantic = Edge
              }
            , { name = "prop2"
              , rpType = RPIBool
              , semantic = Edge
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
