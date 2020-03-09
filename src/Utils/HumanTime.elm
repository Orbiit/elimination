module Utils.HumanTime exposing (..)

import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute)
import Time
import Iso8601

display : Int -> Html msg
display ms =
  let
    time = Time.millisToPosix ms
    iso = Iso8601.fromTime time
  in
  node "local-time"
    [ attribute "datetime" iso
    , attribute "year" "numeric"
    , attribute "month" "long"
    , attribute "day" "numeric"
    , attribute "weekday" "long"
    , attribute "hour" "numeric"
    , attribute "minute" "2-digit"
    ]
    [ text iso ]
