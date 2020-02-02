module Utils exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Http
import Json.Encode as E
import Json.Decode as D

type Char
  = Middot

char : Char -> String
char specialChar =
  case specialChar of
    Middot ->
      "\u{00b7}"

extLink : String -> String -> String -> Html msg
extLink linkText url className =
  a [ A.href url, A.rel "noopener noreferrer", A.target "_blank", A.class className ]
    [ text linkText ]

host : String
host = "https://sheep.thingkingland.app/assassin/"

type alias Msg a msg = (Result Http.Error a -> msg)

get : String -> Maybe String -> Msg a msg -> D.Decoder a -> Cmd msg
get path session msg decoder =
  Http.request
    { method = "GET"
    , headers = []
    , url = host ++ path
    , body = Http.emptyBody
    , expect = Http.expectJson msg decoder
    , timeout = Nothing
    , tracker = Nothing
    }

post : String -> Maybe String -> Msg a msg -> E.Value -> D.Decoder a -> Cmd msg
post path session msg body decoder =
  Http.request
    { method = "GET"
    , headers = []
    , url = host ++ path
    , body = Http.jsonBody body
    , expect = Http.expectJson msg decoder
    , timeout = Nothing
    , tracker = Nothing
    }
