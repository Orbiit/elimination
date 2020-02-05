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
  , loginLoading : Bool
  , loginProblem : Maybe String
  , signUpLoading : Bool
  , signUpProblem : Maybe String
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
      ( { model | loginLoading = True, loginProblem = Nothing }
      , Api.Command (Api.login
        (Tuple.first model.values.loginUsername)
        (Tuple.first model.values.loginPassword)
        (NewSession LoginMethod)
      ))
    SignUp ->
      ( { model | signUpLoading = True, signUpProblem = Nothing }
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
          let
            values = model.values
          in
            ( case method of
              LoginMethod ->
                { model | loginLoading = False, values = { values | loginPassword = ("", False) } }
              SignUpMethod ->
                { model | signUpLoading = False, values = { values | signUpPassword = ("", False) } }
            , Api.ChangeSession (Api.SignedIn { username = username, session = newSession })
            )
        Err error ->
          ( case method of
            LoginMethod ->
              { model | loginLoading = False, loginProblem = Just (Tuple.second error) }
            SignUpMethod ->
              { model | signUpLoading = False, signUpProblem = Just (Tuple.second error) }
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
            , input
              [ A.class "button submit-btn"
              , A.classList [ ("loading", model.loginLoading) ]
              , A.type_ "submit"
              , A.value "Log in"
              , A.disabled model.loginLoading
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
              , value = Tuple.first model.values.signUpUsername
              , validate = Api.Validate.usernameOk
              , maxChars = Nothing
              , storeValueMsg = Change SignUpUsername
              }
            , Utils.myInput
              { labelText = "Full name"
              , sublabel = Api.Validate.nameLabel
              , type_ = "text"
              , placeholder = "Billy Chelontuvier"
              , value = Tuple.first model.values.signUpName
              , validate = Api.Validate.nameOk
              , maxChars = Nothing
              , storeValueMsg = Change SignUpName
              }
            , Utils.myInput
              { labelText = "Email"
              , sublabel = Api.Validate.emailLabel
              , type_ = "email"
              , placeholder = "billygamer5@example.com"
              , value = Tuple.first model.values.signUpEmail
              , validate = Api.Validate.emailOk
              , maxChars = Nothing
              , storeValueMsg = Change SignUpEmail
              }
            , Utils.myInput
              { labelText = "Password"
              , sublabel = Api.Validate.passwordLabel
              , type_ = "password"
              , placeholder = "hunter2"
              , value = Tuple.first model.values.signUpPassword
              , validate = Api.Validate.passwordOk
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
            , input
              [ A.class "button submit-btn"
              , A.classList [ ("loading", model.signUpLoading) ]
              , A.type_ "submit"
              , A.value "Sign up"
              , A.disabled model.signUpLoading
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
