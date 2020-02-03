module Base exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (stopPropagationOn)
import Json.Decode as D

import Api
import Utils

type HeaderWindow
  = SignUp
  | Login
  | Notifications
  | None

type Input
  = LoginUsername
  | LoginPassword
  | SignUpUsername
  | SignUpName
  | SignUpEmail
  | SignUpPassword
  | SignUpPasswordAgain

type alias Model =
  { open : HeaderWindow
  , values :
    { loginUsername : (String, Bool)
    , loginPassword : (String, Bool)
    , signUpUsername : (String, Bool)
    , signUpName : (String, Bool)
    , signUpEmail : (String, Bool)
    , signUpPassword : (String, Bool)
    , signUpPasswordAgain : (String, Bool)
    }
  }

init : Api.Session -> Model
init _ =
  { open = None
  , values =
    { loginUsername = ("", False)
    , loginPassword = ("", False)
    , signUpUsername = ("", False)
    , signUpName = ("", False)
    , signUpEmail = ("", False)
    , signUpPassword = ("", False)
    , signUpPasswordAgain = ("", False)
    }
  }

type Msg
  = Open HeaderWindow
  | Close
  | DontClose
  | Change Input (String -> String) String

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Open window ->
      ({ model | open = window }, Cmd.none)
    Close ->
      ({ model | open = None }, Cmd.none)
    DontClose ->
      (model, Cmd.none)
    Change input validate value ->
      let
        ok = validate value == ""
        values = model.values
      in
        ({ model
        | values =
          case input of
            LoginUsername ->
              { values | loginUsername = (value, ok) }
            LoginPassword ->
              { values | loginPassword = (value, ok) }
            SignUpUsername ->
              { values | signUpUsername = (value, ok) }
            SignUpName ->
              { values | signUpName = (value, ok) }
            SignUpEmail ->
              { values | signUpEmail = (value, ok) }
            SignUpPassword ->
              { values | signUpPassword = (value, ok) }
            SignUpPasswordAgain ->
              { values | signUpPasswordAgain = (value, ok) }
        }, Cmd.none)

-- TODO: abstract header windows etc into helper function
makeHeader : Api.Session -> Model -> List (Html Msg)
makeHeader session model =
  [ header [ A.class "header" ]
    ([ a [ A.href "?", A.class "site-name link" ]
      [ text "Elimination" ]
    , span [ A.class "flex" ] []
    ]
    ++ case session of
      Api.SignedIn { username } ->
        [ div
          [ A.class "header-window-wrapper"
          , A.classList [ ("open", model.open == Notifications) ]
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          ]
          [ button
            [ A.class "icon-btn header-btn notif-btn"
            , stopPropagationOn "click" (D.succeed (Open Notifications, True))
            ]
            [ text "Notifications" ]
          , div [ A.class "header-window notifs" ]
            [ h2 [ A.class "notif-header" ]
              [ text "Notifications"
              , span [ A.class "flex" ]
                []
              , label [ A.class "email-notifs" ]
                [ input [ A.class "checkbox", A.type_ "checkbox" ]
                  []
                , text "Send notifications to my email"
                ]
              ]
            , div [ A.class "notif" ]
              [ span [ A.class "notif-timestamp" ]
                [ text "2020-01-13 at 16:01" ]
              , span [ A.class "notif-msg" ]
                [ a [ A.class "link", A.href "./user.html" ]
                  [ text "Jame Sooth" ]
                , text "has just eliminated you in "
                , a [ A.class "link", A.href "./game.html" ]
                  [ text "Practice elimination round 6" ]
                , text "."
                ]
              ]
            , div [ A.class "notif" ]
              [ span [ A.class "notif-timestamp" ]
                [ text "2019-12-01 at 06:39" ]
              , span [ A.class "notif-msg" ]
                [ text "You were kicked from "
                , a [ A.class "link", A.href "./game.html" ]
                  [ text "Test round. DO NOT JOIN." ]
                , text "because \"boomer\"."
                ]
              ]
            ]
          ]
        , a [ A.class "link username", A.href "./user-settings.html" ]
          [ text username ]
        ]
      Api.SignedOut ->
        [ div
          [ A.class "header-window-wrapper"
          , A.classList [ ("open", model.open == Login) ]
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          ]
          [ button
            [ A.class "header-btn auth-btn"
            , stopPropagationOn "click" (D.succeed (Open Login, True))
            ]
            [ text "Log in" ]
          , form [ A.action "./front-page.html", A.class "header-window" ]
            [ Utils.myInput
              { labelText = "Username"
              , sublabel = ""
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = Tuple.first model.values.loginUsername
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change LoginUsername
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = ""
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.loginPassword
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change LoginPassword
              }
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Log in" ]
              []
            ]
          ]
        , div
          [ A.class "header-window-wrapper"
          , A.classList [ ("open", model.open == SignUp) ]
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          ]
          [ button
            [ A.class "header-btn auth-btn"
            , stopPropagationOn "click" (D.succeed (Open SignUp, True))
            ]
            [ text "Sign up" ]
          , form [ A.action "./front-page.html", A.class "header-window" ]
            [ Utils.myInput
              { labelText = "Username"
              , sublabel = "Only letters, digits, underscores, and hyphens are allowed. This cannot be changed later."
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = Tuple.first model.values.signUpUsername
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change SignUpUsername
              }
            , Utils.myInput
              { labelText = "Full name"
              , sublabel = "Allows others to be able to find you for elimination, which makes the game fair."
              , type_ = "text"
              , placeholder = "Billy Chelontuvier"
              , value = Tuple.first model.values.signUpName
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change SignUpName
              }
            , Utils.myInput
              { labelText = "Email"
              , sublabel = "For password reset forms. You can also turn on email notifications if you want."
              , type_ = "email"
              , placeholder = "billygamer5@example.com"
              , value = Tuple.first model.values.signUpEmail
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change SignUpEmail
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = "Must be at least 3 poop emoji long."
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.signUpPassword
              , validate = \_ -> ""
              , maxChars = Nothing
              , storeValueMsg = Change SignUpPassword
              }
            , Utils.myInput
              { labelText = "Password again"
              , sublabel = ""
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.signUpPasswordAgain
              , validate = \value ->
                if value /= Tuple.first model.values.signUpPassword then
                  "Passwords do not match!"
                else
                  ""
              , maxChars = Nothing
              , storeValueMsg = Change SignUpPasswordAgain
              }
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Sign up" ]
              []
            ]
          ]
        ])
  ]

makeFooter : List (Html msg)
makeFooter =
  [ footer [ A.class "footer" ]
    [ span []
      [ text "Created by the creators of "
      , Utils.extLink "UGWA" "https://gunn.app/" "link"
      , text "."
      ]
    , span [ A.class "flex" ] []
    , span []
      [ a [ A.href "?about", A.class "link" ]
        [ text "About" ]
      , text (" " ++ Utils.char Utils.Middot ++ " ")
      , a [ A.href "?privacy", A.class "link" ]
        [ text "Privacy policy" ]
      , text (" " ++ Utils.char Utils.Middot ++ " ")
      , a [ A.href "?terms", A.class "link" ]
        [ text "Terms of use" ]
      ]
    ]
  ]
