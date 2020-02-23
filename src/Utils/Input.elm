module Utils.Input exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events as Events
import Json.Decode as D

type alias MyInputOptions msg =
  { labelText : String
  , sublabel : List (Html msg)
  , type_ : String
  , placeholder : String
  , value : String
  , name : String
  , validate : String -> Maybe String
  , maxChars : Maybe Int
  , height : Maybe String
  , id : Maybe String
  }

myInputDefaults : MyInputOptions msg
myInputDefaults =
  { labelText = ""
  , sublabel = []
  , type_ = "text"
  , placeholder = ""
  , value = ""
  , name = ""
  , validate = \_ -> Nothing
  , maxChars = Nothing
  , height = Nothing
  , id = Nothing
  }

type alias MyInputMsg =
  { value : String
  , validate : String -> Maybe String
  , scrollWidth : Float
  , scrollHeight : Float
  }

makeMyInputMsg : (MyInputMsg -> msg) -> String -> (String -> Maybe String) -> Float -> Float -> msg
makeMyInputMsg msg value validate scrollWidth scrollHeight =
  msg (MyInputMsg value validate scrollWidth scrollHeight)

myInput : (MyInputMsg -> msg) -> MyInputOptions msg -> Html.Html msg
myInput msg options =
  label
    [ A.class "input-wrapper"
    , A.classList
      [ ( "error"
        , case options.validate options.value of
          Just error ->
            not (String.isEmpty error)
          Nothing ->
            False
        )
      ]
    ]
    [ span [ A.class "label" ]
      [ text options.labelText ]
    , div [ A.class "input" ] <|
      [ (if options.type_ == "textarea" then textarea else input)
        ([ A.placeholder options.placeholder
        , A.value options.value
        -- https://gist.github.com/ohanhi/cb42ba2587fefbdae6962518176d114a
        , Events.on "input" <|
          D.map4 (makeMyInputMsg msg)
            (D.at [ "target", "value" ] D.string)
            (D.succeed options.validate)
            (D.at [ "target", "scrollWidth" ] D.float)
            (D.at [ "target", "scrollHeight" ] D.float)
        , A.name options.name
        ]
          ++ (if options.type_ == "textarea" then [] else [ A.type_ options.type_ ])
          ++ (case options.height of
            Just height -> [ A.style "height" height ]
            Nothing -> [])
          ++ (case options.id of
            Just id -> [ A.id id ]
            Nothing -> []))
        []
      ]
        ++ case options.maxChars of
          Just chars ->
            if toFloat (String.length options.value) / toFloat chars >= 0.7 then
              [ span [ A.class "count" ]
                [ text (String.fromInt (String.length options.value) ++ "/" ++ String.fromInt chars) ]
              ]
            else
              []
          Nothing ->
            []
    , span [ A.class "problem" ]
      (case options.validate options.value of
        Just error ->
          if String.isEmpty error then
            []
          else
            [ text error ]
        Nothing ->
          []
      )
    , span [ A.class "sublabel" ]
      options.sublabel
    ]

type alias InputState = { value : String, original : String, valid : Bool }

initInputState : InputState
initInputState =
  { value = "", original = "", valid = False }

inputState : String -> InputState
inputState value =
  { value = value, original = value, valid = True }

updateValue : InputState -> String -> Bool -> InputState
updateValue state value ok =
  { state | value = value, valid = ok }
