module Base exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (stopPropagationOn, onSubmit)
import Json.Decode as D
import Http

import Api
import Api.Validate
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

type alias InputState = { value : String, valid : Bool }

initInputState : InputState
initInputState =
  { value = "", valid = False }

type alias Model =
  { open : HeaderWindow
  , values :
    { loginUsername : InputState
    , loginPassword : InputState
    , signUpUsername : InputState
    , signUpName : InputState
    , signUpEmail : InputState
    , signUpPassword : InputState
    , signUpPasswordAgain : InputState
    }
  , loginLoading : Bool
  , loginProblem : Maybe String
  , signUpLoading : Bool
  , signUpProblem : Maybe String
  }

init : Api.Session -> Model
init _ =
  { open = None
  , values =
    { loginUsername = initInputState
    , loginPassword = initInputState
    , signUpUsername = initInputState
    , signUpName = initInputState
    , signUpEmail = initInputState
    , signUpPassword = initInputState
    , signUpPasswordAgain = initInputState
    }
  , loginLoading = False
  , loginProblem = Nothing
  , signUpLoading = False
  , signUpProblem = Nothing
  }

type Msg
  = Open HeaderWindow
  | Close
  | DontClose
  | Change Input (String -> Maybe String) String
  | Login
  | SignUp
  | NewSession AuthMethod String (Result Utils.HttpError Api.SessionID)

update : Msg -> Api.Session -> Model -> (Model, Api.PageCmd Msg)
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
        valid = case validate value of
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
              { values | loginUsername = { value = value, valid = valid } }
            LoginPassword ->
              { values | loginPassword = { value = value, valid = valid } }
            SignUpUsername ->
              { values | signUpUsername = { value = value, valid = valid } }
            SignUpName ->
              { values | signUpName = { value = value, valid = valid } }
            SignUpEmail ->
              { values | signUpEmail = { value = value, valid = valid } }
            SignUpPassword ->
              { values | signUpPassword = { value = value, valid = valid } }
            SignUpPasswordAgain ->
              { values | signUpPasswordAgain = { value = value, valid = valid } }
        }, Api.Command Cmd.none)
    Login ->
      ( { model | loginLoading = True, loginProblem = Nothing }
      , Api.Command (Api.login
        (model.values.loginUsername.value)
        (model.values.loginPassword.value)
        (NewSession LoginMethod)
      ))
    SignUp ->
      ( { model | signUpLoading = True, signUpProblem = Nothing }
      , Api.Command (Api.createUser
        { username = (model.values.signUpUsername.value)
        , name = (model.values.signUpName.value)
        , password = (model.values.signUpPassword.value)
        , email = (model.values.signUpEmail.value)
        , bio = ""
        }
        (NewSession SignUpMethod)
      ))
    NewSession method username sessionResult ->
      case sessionResult of
        Ok newSession ->
          let
            values = model.values
          in
            ( case method of
              LoginMethod ->
                { model | loginLoading = False, values = { values | loginPassword = initInputState } }
              SignUpMethod ->
                { model | signUpLoading = False, values = { values | signUpPassword = initInputState } }
            , Api.ChangeSession (Api.SignedIn { username = username, session = newSession })
            )
        Err (_, error) ->
          ( case method of
            LoginMethod ->
              { model | loginLoading = False, loginProblem = Just error }
            SignUpMethod ->
              { model | signUpLoading = False, signUpProblem = Just error }
          , Api.Command Cmd.none
          )

headerWindow : Model -> String -> String -> HeaderWindow -> List (Html Msg) -> Html Msg
headerWindow model btnClass btnLabel window windowContent =
  div
    [ A.class "header-window-wrapper"
    , A.classList [ ("open", model.open == window) ]
    , stopPropagationOn "click" (D.succeed (DontClose, True))
    ] <|
    button
      [ A.class btnClass
      , stopPropagationOn "click" (D.succeed (Open window, True))
      ]
      [ text btnLabel ]
      :: windowContent

makeHeader : Api.Session -> Model -> List (Html Msg)
makeHeader session model =
  [ header [ A.class "header" ] <|
    [ a [ A.href "?", A.class "site-name link" ]
      [ text "Elimination" ]
    , span [ A.class "flex" ] []
    ]
    ++ case session of
      Api.SignedIn { username } ->
        [ headerWindow model "icon-btn header-btn notif-btn" "Notifications" Notifications <|
          [ div [ A.class "header-window notifs" ]
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
        , a [ A.class "link username", A.href "?settings" ]
          [ text username ]
        ]
      Api.SignedOut ->
        [ headerWindow model "header-btn auth-btn" "Log in" LoginWindow <|
          [ form [ A.class "header-window", onSubmit Login ] <|
            [ Utils.myInput
              { labelText = "Username"
              , sublabel = ""
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = model.values.loginUsername.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Nothing
              , maxChars = Nothing
              , storeValueMsg = Change LoginUsername
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = ""
              , type_ = "password"
              , placeholder = "hunter2"
              , value = model.values.loginPassword.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Nothing
              , maxChars = Nothing
              , storeValueMsg = Change LoginPassword
              }
            , input
              [ A.class "button submit-btn"
              , A.classList [ ("loading", model.loginLoading) ]
              , A.type_ "submit"
              , A.value "Log in"
              , A.disabled (model.loginLoading || not
                (model.values.loginUsername.valid &&
                model.values.loginPassword.valid))
              ]
              []
            ]
            ++ case model.loginProblem of
              Just errorText ->
                [ span [ A.class "problematic-error" ]
                  [ text errorText ] ]
              Nothing ->
                []
          ]
        , headerWindow model "header-btn auth-btn" "Sign up" SignUpWindow <|
          [ form [ A.class "header-window", onSubmit SignUp ] <|
            [ Utils.myInput
              { labelText = "Username"
              , sublabel = Api.Validate.usernameLabel
              , type_ = "text"
              , placeholder = "billygamer5"
              , value = model.values.signUpUsername.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.usernameOk value
              , maxChars = Just 20
              , storeValueMsg = Change SignUpUsername
              }
            , Utils.myInput
              { labelText = "Full name"
              , sublabel = Api.Validate.nameLabel
              , type_ = "text"
              , placeholder = "Billy Chelontuvier"
              , value = model.values.signUpName.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.nameOk value
              , maxChars = Just 50
              , storeValueMsg = Change SignUpName
              }
            , Utils.myInput
              { labelText = "Email"
              , sublabel = Api.Validate.emailLabel
              , type_ = "email"
              , placeholder = "billygamer5@example.com"
              , value = model.values.signUpEmail.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.emailOk value
              , maxChars = Just 320
              , storeValueMsg = Change SignUpEmail
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = Api.Validate.passwordLabel
              , type_ = "password"
              , placeholder = "hunter2"
              , value = model.values.signUpPassword.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.passwordOk value
              , maxChars = Just 200
              , storeValueMsg = Change SignUpPassword
              }
            , Utils.myInput
              { labelText = "Password again"
              , sublabel = ""
              , type_ = "password"
              , placeholder = "hunter2"
              , value = model.values.signUpPasswordAgain.value
              , validate = \value ->
                if value /= model.values.signUpPassword.value then
                  Just "Passwords do not match!"
                else
                  Nothing
              , maxChars = Nothing
              , storeValueMsg = Change SignUpPasswordAgain
              }
            , input
              [ A.class "button submit-btn"
              , A.classList [ ("loading", model.signUpLoading) ]
              , A.type_ "submit"
              , A.value "Sign up"
              , A.disabled (model.signUpLoading || not
                (model.values.signUpUsername.valid &&
                model.values.signUpName.valid &&
                model.values.signUpEmail.valid &&
                model.values.signUpPassword.valid &&
                model.values.signUpPasswordAgain.valid))
              ]
              []
            ]
            ++ case model.signUpProblem of
              Just errorText ->
                [ span [ A.class "problematic-error" ]
                  [ text errorText ] ]
              Nothing ->
                []
          ]
        ]
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
