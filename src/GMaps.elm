port module GMaps exposing (..)

import SharedModels exposing (..)



-- PORTS


port registerNode : ( Int, List GMPos ) -> Cmd msg


port registerPath : ( Int, List (List GMPos) ) -> Cmd msg


port show : Int -> Cmd msg


port hide : Int -> Cmd msg


port moveMap : GMPos -> Cmd msg


port mapMoved : (GMPos -> msg) -> Sub msg
