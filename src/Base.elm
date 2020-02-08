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
  , loginUsername : InputState
  , loginPassword : InputState
  , signUpUsername : InputState
  , signUpName : InputState
  , signUpEmail : InputState
  , signUpPassword : InputState
  , signUpPasswordAgain : InputState
  , loginLoading : Bool
  , loginProblem : Maybe String
  , signUpLoading : Bool
  , signUpProblem : Maybe String
  }

init : Api.Session -> Model
init _ =
  { open = None
  , loginUsername = initInputState
  , loginPassword = initInputState
  , signUpUsername = initInputState
  , signUpName = initInputState
  , signUpEmail = initInputState
  , signUpPassword = initInputState
  , signUpPasswordAgain = initInputState
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

update : Msg -> Api.Session -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg session model =
  case msg of
    Open window ->
      ({ model | open = window }, Cmd.none, Api.None)
    Close ->
      ({ model | open = None }, Cmd.none, Api.None)
    DontClose ->
      (model, Cmd.none, Api.None)
    Change input validate value ->
      let
        valid = case validate value of
          Just _ ->
            False
          Nothing ->
            True
      in
        ( case input of
          LoginUsername ->
            { model | loginUsername = { value = value, valid = valid } }
          LoginPassword ->
            { model | loginPassword = { value = value, valid = valid } }
          SignUpUsername ->
            { model | signUpUsername = { value = value, valid = valid } }
          SignUpName ->
            { model | signUpName = { value = value, valid = valid } }
          SignUpEmail ->
            { model | signUpEmail = { value = value, valid = valid } }
          SignUpPassword ->
            { model | signUpPassword = { value = value, valid = valid } }
          SignUpPasswordAgain ->
            { model | signUpPasswordAgain = { value = value, valid = valid } }
        , Cmd.none
        , Api.None
        )
    Login ->
      ( { model | loginLoading = True, loginProblem = Nothing }
      , Api.login model.loginUsername.value model.loginPassword.value
        (NewSession LoginMethod)
      , Api.None
      )
    SignUp ->
      ( { model | signUpLoading = True, signUpProblem = Nothing }
      , Api.createUser
        { username = model.signUpUsername.value
        , name = model.signUpName.value
        , password = model.signUpPassword.value
        , email = model.signUpEmail.value
        , bio = ""
        }
        (NewSession SignUpMethod)
      , Api.None
      )
    NewSession method username sessionResult ->
      case sessionResult of
        Ok newSession ->
          ( case method of
            LoginMethod ->
              { model | loginLoading = False, loginPassword = initInputState }
            SignUpMethod ->
              { model | signUpLoading = False, signUpPassword = initInputState }
          , Cmd.none
          , Api.ChangeSession (Api.SignedIn { username = username, session = newSession })
          )
        Err (_, error) ->
          ( case method of
            LoginMethod ->
              { model | loginLoading = False, loginProblem = Just error }
            SignUpMethod ->
              { model | signUpLoading = False, signUpProblem = Just error }
          , Cmd.none
          , Api.None
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
              , value = model.loginUsername.value
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
              , value = model.loginPassword.value
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
                (model.loginUsername.valid &&
                model.loginPassword.valid))
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
              , value = model.signUpUsername.value
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
              , value = model.signUpName.value
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
              , value = model.signUpEmail.value
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
              , value = model.signUpPassword.value
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
              , value = model.signUpPasswordAgain.value
              , validate = \value ->
                if value /= model.signUpPassword.value then
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
                (model.signUpUsername.valid &&
                model.signUpName.valid &&
                model.signUpEmail.valid &&
                model.signUpPassword.valid &&
                model.signUpPasswordAgain.valid))
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
