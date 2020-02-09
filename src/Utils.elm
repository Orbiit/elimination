module Utils exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events
import Http
import Json.Encode as E
import Json.Decode as D
import Time

filter : List (Maybe a) -> List a
filter list =
  List.filterMap identity list

weekdayNameEnglish : Time.Weekday -> String
weekdayNameEnglish weekday =
  case weekday of
    Time.Mon -> "Monday"
    Time.Tue -> "Tuesday"
    Time.Wed -> "Wednesday"
    Time.Thu -> "Thursday"
    Time.Fri -> "Friday"
    Time.Sat -> "Saturday"
    Time.Sun -> "Sunday"

monthNameEnglish : Time.Month -> String
monthNameEnglish month =
  case month of
    Time.Jan -> "January"
    Time.Feb -> "February"
    Time.Mar -> "March"
    Time.Apr -> "April"
    Time.May -> "May"
    Time.Jun -> "June"
    Time.Jul -> "July"
    Time.Aug -> "August"
    Time.Sep -> "September"
    Time.Oct -> "October"
    Time.Nov -> "November"
    Time.Dec -> "December"

monthName : Time.Month -> String
monthName month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

displayTime : Time.Zone -> Int -> String
displayTime zone ms =
  let
    time = Time.millisToPosix ms
  in
    String.fromInt (Time.toYear zone time) ++ "-"
    ++ monthName (Time.toMonth zone time) ++ "-"
    ++ String.padLeft 2 '0' (String.fromInt (Time.toDay zone time))
    ++ " (" ++ weekdayNameEnglish (Time.toWeekday zone time) ++ ") at "
    ++ String.padLeft 2 '0' (String.fromInt (Time.toHour zone time)) ++ ":"
    ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))

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
          Just error ->
            not (String.isEmpty error)
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
            if toFloat (String.length value) / toFloat chars >= 0.7 then
              [ span [ A.class "count" ]
                [ text (String.fromInt (String.length value) ++ "/" ++ String.fromInt chars) ]
              ]
            else
              []
          Nothing ->
            [])
    , span [ A.class "problem" ]
      (case validate value of
        Just error ->
          if String.isEmpty error then
            []
          else
            [ text error ]
        Nothing ->
          []
      )
    , span [ A.class "sublabel" ]
      [ text sublabel ]
    ]

type alias InputState = { value : String, original : String, valid : Bool }

initInputState : InputState
initInputState =
  { value = "", original = "", valid = True }

inputState : String -> InputState
inputState value =
  { value = value, original = value, valid = True }

updateValue : InputState -> String -> Bool -> InputState
updateValue state value ok =
  { state | value = value, valid = ok }

host : String
host = "http://localhost:3000/assassin/"
-- host = "https://sheep.thingkingland.app/assassin/"

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
    Err parseError ->
      "Supposedly something went wrong, but the server didn't articulate well enough about it.\n"
        ++ D.errorToString parseError

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
      Err (ErrorStatusText "Offline", "Either you or the server is offline.")
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
        Err parseError ->
          Err
            ( ErrorStatusText "Malformed JSON"
            , "The server spoke in a different language, and we couldn't understand it.\n"
              ++ D.errorToString parseError
            )

get : String -> Maybe String -> (Result HttpError a -> msg) -> D.Decoder a -> Cmd msg
get path session msg decoder =
  Http.request
    { method = "GET"
    , headers =
      [ Http.header "X-Requested-With" "XMLHttpRequest" ] ++
        case session of
          Just sessionID ->
            [ Http.header "X-Session-Id" sessionID ]
          Nothing ->
            []
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
    , headers =
      [ Http.header "X-Requested-With" "XMLHttpRequest" ] ++
        case session of
          Just sessionID ->
            [ Http.header "X-Session-Id" sessionID ]
          Nothing ->
            []
    , url = host ++ path
    , body = Http.jsonBody body
    , expect = Http.expectStringResponse msg (parseResponse decoder)
    , timeout = Nothing
    , tracker = Nothing
    }
