module Base exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (stopPropagationOn, onSubmit)
import Json.Decode as D
import Http

import Api
import Utils

type HeaderWindow
  = SignUpWindow
  | LoginWindow
  | Notifications
  | None

type AuthMethod
  = LoginMethod
  | SignUpMethod

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
  , problems :
    { login : Maybe String
    , signUp : Maybe String
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
  , problems =
    { login = Nothing
    , signUp = Nothing
    }
  }

type Msg
  = Open HeaderWindow
  | Close
  | DontClose
  | Change Input (String -> Maybe String) String
  | Login
  | SignUp
  | NewSession AuthMethod String (Result String String)

update : Msg -> Api.Session -> Model -> (Model, Api.SessionOrCmd Msg)
update msg session model =
  case msg of
    Open window ->
      ({ model | open = window }, Api.Command Cmd.none)
    Close ->
      ({ model | open = None }, Api.Command Cmd.none)
    DontClose ->
      (model, Api.Command Cmd.none)
    Change input validate value ->
      let
        ok = case validate value of
          Just _ ->
            False
          Nothing ->
            True
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
        }, Api.Command Cmd.none)
    Login ->
      ( model
      , Api.Command (Api.login
        (Tuple.first model.values.loginUsername)
        (Tuple.first model.values.loginPassword)
        (NewSession LoginMethod)
      ))
    SignUp ->
      ( model
      , Api.Command (Api.createUser
        { username = (Tuple.first model.values.signUpUsername)
        , name = (Tuple.first model.values.signUpName)
        , password = (Tuple.first model.values.signUpPassword)
        , email = (Tuple.first model.values.signUpEmail)
        , bio = ""
        }
        (NewSession SignUpMethod)
      ))
    NewSession method username sessionResult ->
      case sessionResult of
        Ok newSession ->
          (model, Api.ChangeSession (Api.SignedIn { username = username, session = newSession }))
        Err error ->
          let
            problems = model.problems
          in
            ( { model
              | problems = case method of
                LoginMethod ->
                  { problems | login = Just error }
                SignUpMethod ->
                  { problems | signUp = Just error }
              }
            , Api.Command Cmd.none
            )

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
          , A.classList [ ("open", model.open == LoginWindow) ]
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          ]
          [ button
            [ A.class "header-btn auth-btn"
            , stopPropagationOn "click" (D.succeed (Open LoginWindow, True))
            ]
            [ text "Log in" ]
          , form [ A.class "header-window", onSubmit Login ]
            ([ Utils.myInput
              { labelText = "Username"
              , sublabel = ""
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = Tuple.first model.values.loginUsername
              , validate = \_ -> Nothing
              , maxChars = Nothing
              , storeValueMsg = Change LoginUsername
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = ""
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.loginPassword
              , validate = \_ -> Nothing
              , maxChars = Nothing
              , storeValueMsg = Change LoginPassword
              }
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Log in" ]
              []
            ]
            ++ case model.problems.login of
              Just errorText ->
                [ span [ A.class "problematic-error" ]
                  [ text errorText ] ]
              Nothing ->
                []
            )
          ]
        , div
          [ A.class "header-window-wrapper"
          , A.classList [ ("open", model.open == SignUpWindow) ]
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          ]
          [ button
            [ A.class "header-btn auth-btn"
            , stopPropagationOn "click" (D.succeed (Open SignUpWindow, True))
            ]
            [ text "Sign up" ]
          , form [ A.class "header-window", onSubmit SignUp ]
            ([ Utils.myInput
              { labelText = "Username"
              , sublabel = "Only letters, digits, underscores, and hyphens are allowed. This cannot be changed later."
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = Tuple.first model.values.signUpUsername
              , validate = \_ -> Nothing
              , maxChars = Nothing
              , storeValueMsg = Change SignUpUsername
              }
            , Utils.myInput
              { labelText = "Full name"
              , sublabel = "Allows others to be able to find you for elimination, which makes the game fair."
              , type_ = "text"
              , placeholder = "Billy Chelontuvier"
              , value = Tuple.first model.values.signUpName
              , validate = \_ -> Nothing
              , maxChars = Nothing
              , storeValueMsg = Change SignUpName
              }
            , Utils.myInput
              { labelText = "Email"
              , sublabel = "For password reset forms. You can also turn on email notifications if you want."
              , type_ = "email"
              , placeholder = "billygamer5@example.com"
              , value = Tuple.first model.values.signUpEmail
              , validate = \_ -> Nothing
              , maxChars = Nothing
              , storeValueMsg = Change SignUpEmail
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = "Must be at least 3 poop emoji long."
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.signUpPassword
              , validate = \_ -> Nothing
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
                  Just "Passwords do not match!"
                else
                  Nothing
              , maxChars = Nothing
              , storeValueMsg = Change SignUpPasswordAgain
              }
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Sign up" ]
              []
            ]
            ++ case model.problems.signUp of
              Just errorText ->
                [ span [ A.class "problematic-error" ]
                  [ text errorText ] ]
              Nothing ->
                []
            )
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
