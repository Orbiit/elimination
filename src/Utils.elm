module Utils exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events
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

myInput :
  { labelText : String
  , sublabel : String
  , type_ : String
  , placeholder : String
  , value : String
  , validate : String -> Maybe String
  , maxChars : Maybe Int
  , storeValueMsg : (String -> Maybe String) -> String -> msg
  } -> Html.Html msg
myInput { labelText, sublabel, type_, placeholder, value, validate, maxChars, storeValueMsg } =
  label
    [ A.class "input-wrapper"
    , A.classList
      [ ( "error"
        , case validate value of
          Just _ ->
            True
          Nothing ->
            False
        )
      ]
    ]
    [ span [ A.class "label" ]
      [ text labelText ]
    , div [ A.class "input" ]
      ([ (if type_ == "textarea" then textarea else input)
        ([ A.placeholder placeholder, A.value value, Events.onInput (storeValueMsg validate) ]
          ++ if type_ == "textarea" then [] else [ A.type_ type_ ])
        []
      ]
        ++ case maxChars of
          Just chars ->
            [ span [ A.class "count" ]
              [ text ("11/" ++ String.fromInt chars) ]
            ]
          Nothing ->
            [])
    , span [ A.class "problem" ]
      (case validate value of
        Just error ->
          [ text error ]
        Nothing ->
          []
      )
    , span [ A.class "sublabel" ]
      [ text sublabel ]
    ]

host : String
host = "https://sheep.thingkingland.app/assassin/"

type ErrorStatus
  = ErrorStatusText String
  | StatusCode Int
type alias ErrorMessage = String
type alias HttpError = (ErrorStatus, ErrorMessage)

parseWucky : String -> ErrorMessage
parseWucky body =
  case D.decodeString (D.field "mistake" D.string) body of
    Ok wucky ->
      wucky
    Err _ ->
      "Supposedly something went wrong, but the server didn't articulate well enough about it."

parseResponse : D.Decoder a -> Http.Response String -> Result HttpError a
parseResponse decoder response =
  case response of
    Http.BadUrl_ _ ->
      Err
        ( ErrorStatusText "Bad URL"
        , "The request made to the server was done awkwardly, so the server didn't know what to do."
        )
    Http.Timeout_ ->
      Err (ErrorStatusText "Timeout", "The server took too long.")
    Http.NetworkError_ ->
      Err (ErrorStatusText "Offline", "You're probably offline.")
    Http.BadStatus_ metadata body ->
      Err
        ( StatusCode metadata.statusCode
        , case metadata.statusCode of
            400 ->
              parseWucky body
            500 ->
              "The server hurt itself in the process of fulfilling your request."
            404 ->
              "The server apparently doesn't know what it's meant to do."
            _ ->
              "The server...????"
        )
    Http.GoodStatus_ _ body ->
      case D.decodeString decoder body of
        Ok value ->
          Ok value
        Err _ ->
          Err
            ( ErrorStatusText "Malformed JSON"
            , "The server spoke in a different language, and we couldn't understand it."
            )

get : String -> Maybe String -> (Result HttpError a -> msg) -> D.Decoder a -> Cmd msg
get path session msg decoder =
  Http.request
    { method = "GET"
    , headers = []
    , url = host ++ path
    , body = Http.emptyBody
    , expect = Http.expectStringResponse msg (parseResponse decoder)
    , timeout = Nothing
    , tracker = Nothing
    }

post : String -> Maybe String -> (Result HttpError a -> msg) -> E.Value -> D.Decoder a -> Cmd msg
post path session msg body decoder =
  Http.request
    { method = "POST"
    , headers = []
    , url = host ++ path
    , body = Http.jsonBody body
    , expect = Http.expectStringResponse msg (parseResponse decoder)
    , timeout = Nothing
    , tracker = Nothing
    }
