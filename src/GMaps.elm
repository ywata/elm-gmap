port module GMaps exposing (..)

import SharedModels exposing (..)



-- PORTS


port registerNode : ( String, List GMPos ) -> Cmd msg


port registerPath : ( String, List (List GMPos) ) -> Cmd msg


port show : String -> Cmd msg


port hide : String -> Cmd msg


port moveMap : GMPos -> Cmd msg


port mapMoved : (GMPos -> msg) -> Sub msg
