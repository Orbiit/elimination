module Base exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (stopPropagationOn, onSubmit, onClick)
import Json.Decode as D
import Http
import Time
import Browser.Dom as Dom
import Task

import Api
import Api.Validate
import Utils exposing (myInputDefaults)

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
  , loginUsername : Utils.InputState
  , loginPassword : Utils.InputState
  , signUpUsername : Utils.InputState
  , signUpName : Utils.InputState
  , signUpEmail : Utils.InputState
  , signUpPassword : Utils.InputState
  , signUpPasswordAgain : Utils.InputState
  , loginLoading : Bool
  , loginProblem : Maybe String
  , signUpLoading : Bool
  , signUpProblem : Maybe String
  , notifs : Api.NotificationResult
  , notifsLoading : Bool
  , notifsProblem : Maybe String
  , markingAsRead : Bool
  }

init : Api.GlobalModel m -> (Model, Cmd Msg)
init semiGlobal =
  ( { open = None
    , loginUsername = Utils.initInputState
    , loginPassword = Utils.initInputState
    , signUpUsername = Utils.initInputState
    , signUpName = Utils.initInputState
    , signUpEmail = Utils.initInputState
    , signUpPassword = Utils.initInputState
    , signUpPasswordAgain = Utils.initInputState
    , loginLoading = False
    , loginProblem = Nothing
    , signUpLoading = False
    , signUpProblem = Nothing
    , notifs =
      { notifications = []
      , end = False
      , unread = 0
      }
    , notifsLoading = True
    , notifsProblem = Nothing
    , markingAsRead = False
    }
  , updateNotifs semiGlobal
  )

clearNotifs : Model -> Model
clearNotifs model =
  { model | notifs = { notifications = [], end = False, unread = 0 } }

type Msg
  = Open HeaderWindow
  | Close
  | Change Input Utils.MyInputMsg
  | Login
  | SignUp
  | NewSession AuthMethod String (Result Utils.HttpError Api.SessionID)
  | Refresh
  | NotificationsLoaded (Result Utils.HttpError Api.NotificationResult)
  | LoadMore
  | MoreNotificationsLoaded (Result Utils.HttpError Api.NotificationResult)
  | MarkAsRead
  | MarkedAsRead (Result Utils.HttpError ())
  | DoNothing

notifsAtATime : Int
notifsAtATime = 5

updateNotifs : Api.GlobalModel m -> Cmd Msg
updateNotifs global =
  Api.notifications global NotificationsLoaded 0 notifsAtATime

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    Open window ->
      ( { model | open = window }
      , Task.attempt (\_ -> DoNothing) <|
        Dom.focus <| case window of
          SignUpWindow -> "sign-up-input"
          LoginWindow -> "login-input"
          Notifications -> "notification-btn"
          None -> ""
      , Api.None
      )
    Close ->
      ({ model | open = None }, Cmd.none, Api.None)
    Change input { validate, value } ->
      let
        valid = case validate value of
          Just _ ->
            False
          Nothing ->
            True
      in
        ( case input of
          LoginUsername ->
            { model | loginUsername = Utils.updateValue model.loginUsername value valid }
          LoginPassword ->
            { model | loginPassword = Utils.updateValue model.loginPassword value valid }
          SignUpUsername ->
            { model | signUpUsername = Utils.updateValue model.signUpUsername value valid }
          SignUpName ->
            { model | signUpName = Utils.updateValue model.signUpName value valid }
          SignUpEmail ->
            { model | signUpEmail = Utils.updateValue model.signUpEmail value valid }
          SignUpPassword ->
            { model | signUpPassword = Utils.updateValue model.signUpPassword value valid }
          SignUpPasswordAgain ->
            { model | signUpPasswordAgain = Utils.updateValue model.signUpPasswordAgain value valid }
        , Cmd.none
        , Api.None
        )
    Login ->
      ( { model | loginLoading = True, loginProblem = Nothing }
      , Api.login
        global
        (NewSession LoginMethod)
        model.loginUsername.value model.loginPassword.value
      , Api.None
      )
    SignUp ->
      ( { model | signUpLoading = True, signUpProblem = Nothing }
      , Api.createUser
        global
        (NewSession SignUpMethod)
        { username = model.signUpUsername.value
        , name = model.signUpName.value
        , password = model.signUpPassword.value
        , email = model.signUpEmail.value
        , bio = ""
        }
      , Api.None
      )
    NewSession method username sessionResult ->
      case sessionResult of
        Ok newSession ->
          ( case method of
            LoginMethod ->
              { model | loginLoading = False, loginPassword = Utils.initInputState }
            SignUpMethod ->
              { model | signUpLoading = False, signUpPassword = Utils.initInputState, signUpPasswordAgain = Utils.initInputState }
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
    Refresh ->
      ( { model | notifsLoading = True, notifsProblem = Nothing }
      , updateNotifs global
      , Api.None
      )
    NotificationsLoaded result ->
      case result of
        Ok notifResult ->
          ({ model | notifsLoading = False, notifs = notifResult, notifsProblem = Nothing }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | notifsLoading = False, notifsProblem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    LoadMore ->
      ( { model | notifsLoading = True, notifsProblem = Nothing }
      , Api.notifications global MoreNotificationsLoaded (List.length model.notifs.notifications) notifsAtATime
      , Api.None
      )
    MoreNotificationsLoaded result ->
      case result of
        Ok notifResult ->
          ({ model | notifsLoading = False, notifs =
            { notifResult
            | notifications =
              model.notifs.notifications ++ notifResult.notifications
            }
          }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | notifsLoading = False, notifsProblem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    MarkAsRead ->
      ( { model | markingAsRead = True, notifsProblem = Nothing }
      , Api.read global MarkedAsRead
      , Api.None
      )
    MarkedAsRead result ->
      case result of
        Ok _ ->
          let
            notifs = model.notifs
          in
            ({ model | markingAsRead = False, notifs =
              { notifs | unread = 0, notifications =
                List.map (\notif -> { notif | read = True }) notifs.notifications
              }
            }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | markingAsRead = False, notifsProblem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    DoNothing ->
      (model, Cmd.none, Api.None)

headerWindow : Model -> String -> List (Html Msg) -> HeaderWindow -> List (Html Msg) -> Html Msg
headerWindow model btnClass btnLabel window windowContent =
  div
    [ A.class "header-window-wrapper"
    , A.classList [ ("open", model.open == window) ]
    , stopPropagationOn "click" (D.succeed (DoNothing, True))
    ] <|
    button
      [ A.class btnClass
      , stopPropagationOn "click" (D.succeed (Open window, True))
      ]
      btnLabel
      :: windowContent

renderNotification : Time.Zone -> Api.NotificationMessage -> Html msg
renderNotification zone { time, read, message } =
  div
    [ A.class "notif"
    , A.classList [ ("unread", not read) ]
    ]
    [ span [ A.class "notif-timestamp" ]
      [ text (Utils.displayTime zone time) ]
    , case message of
      Api.GameStarted gameID gameName ->
        span [ A.class "notif-msg" ]
          [ a [ A.class "link", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text " has started! See the home page for your target."
          ]
      Api.GameEnded gameID gameName winner winnerName ->
        span [ A.class "notif-msg" ]
          [ a [ A.class "link", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text " has ended! "
          , a [ A.class "link", A.href ("?@" ++ winner) ]
            [ text winnerName ]
          , text " was the last one standing."
          ]
      Api.Killed gameID gameName killer killerName ->
        span [ A.class "notif-msg" ]
          [ a [ A.class "link", A.href ("?@" ++ killer) ]
            [ text killerName ]
          , text " has just eliminated you in "
          , a [ A.class "link", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text "."
          ]
      Api.Kicked gameID gameName reason ->
        span [ A.class "notif-msg" ]
          [ text "You were kicked from "
          , a [ A.class "link", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text
            (if reason == "" then " for unknown reasons." else " because \"" ++ reason ++ "\".")
          ]
      Api.Shuffle gameID gameName ->
        span [ A.class "notif-msg" ]
          [ text "The targets of everyone in "
          , a [ A.class "link", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text " has been shuffled. See the home page for your new target."
          ]
      Api.Unknown msgType ->
        span [ A.class "notif-msg" ]
          [ text ("(Indistinct \"" ++ msgType ++ "\" message)") ]
    ]

makeHeader : Api.GlobalModel m -> Model -> Bool -> List (Html Msg)
makeHeader { session, zone } model frontPage =
  [ header
    [ A.class "header"
    , A.classList
      [ ("on-welcome-page"
        , case session of
          Api.SignedIn _ ->
            False
          Api.SignedOut ->
            frontPage
        )
      ]
    ] <|
    [ a [ A.href "?", A.class "site-name link" ]
      [ text "Elimination" ]
    , span [ A.class "flex" ] []
    ]
    ++ case session of
      Api.SignedIn { username } ->
        (if frontPage then
          [ a [ A.class "button create-game-btn", A.href "?create-game" ]
            [ text "Create game" ] ]
        else
          [])
        ++ [ headerWindow model "icon-btn header-btn notif-btn"
          [ text "Notifications ("
          , span [ A.classList [ ("show-unread", model.notifs.unread > 0) ] ]
            [ text (String.fromInt model.notifs.unread) ]
          , text ")" ]
          Notifications
          [ div [ A.class "header-window notifs" ]
            ((h2 [ A.class "notif-header" ]
              [ text "Notifications"
              , span [ A.class "flex" ]
                []
              , button
                [ A.class "button small-btn notif-action-btn"
                , A.classList [ ("loading", model.markingAsRead) ]
                , A.id "notification-btn"
                , A.disabled (model.markingAsRead || model.notifs.unread == 0)
                , onClick MarkAsRead
                ]
                [ text "Mark as read" ]
              , button
                [ A.class "button small-btn notif-action-btn"
                , A.disabled model.notifsLoading
                , onClick Refresh
                ]
                [ text "Refresh" ]
              -- , label [ A.class "email-notifs" ]
              --   [ input [ A.class "checkbox", A.type_ "checkbox" ]
              --     []
              --   , text "Send notifications to my email"
              --   ]
              ])
            :: List.map (renderNotification zone) model.notifs.notifications
            ++ (case model.notifsProblem of
              Just errorText ->
                [ span [ A.class "problematic-error" ]
                  [ text errorText ] ]
              Nothing ->
                [])
            ++ if model.notifsLoading then
              [ button [ A.class "button load-more-btn loading", A.disabled True ]
                [ text "Loading" ] ]
            else if model.notifs.end then
              []
            else
              [ button [ A.class "button load-more-btn", onClick LoadMore ]
                [ text "Load more" ] ])
          ]
        , a [ A.class "link username", A.href "?settings" ]
          [ text username ]
        ]
      Api.SignedOut ->
        [ headerWindow model "header-btn auth-btn" [ text "Log in" ] LoginWindow <|
          [ form [ A.class "header-window", onSubmit Login ] <|
            [ Utils.myInput (Change LoginUsername)
              { myInputDefaults
              | labelText = "Username"
              , placeholder = "billygamer5"
              , name = "username"
              , value = model.loginUsername.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Nothing
              , id = Just "login-input"
              }
            , Utils.myInput (Change LoginPassword)
              { myInputDefaults
              | labelText = "Password"
              , type_ = "password"
              , placeholder = "hunter2"
              , name = "password"
              , value = model.loginPassword.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Nothing
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
        , headerWindow model "header-btn auth-btn" [ text "Sign up" ] SignUpWindow <|
          [ form [ A.class "header-window", onSubmit SignUp ] <|
            [ Utils.myInput (Change SignUpUsername)
              { myInputDefaults
              | labelText = "Username"
              , sublabel = Api.Validate.usernameLabel
              , type_ = "text"
              , placeholder = "billygamer5"
              , name = "username"
              , value = model.signUpUsername.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.usernameOk value
              , maxChars = Just 20
              , id = Just "sign-up-input"
              }
            , Utils.myInput (Change SignUpName)
              { myInputDefaults
              | labelText = "Full name"
              , sublabel = Api.Validate.nameLabel
              , placeholder = "Billy Chelontuvier"
              , value = model.signUpName.value
              , name = "name"
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.nameOk value
              , maxChars = Just 50
              }
            , Utils.myInput (Change SignUpEmail)
              { myInputDefaults
              | labelText = "Email"
              , sublabel = Api.Validate.emailLabel
              , type_ = "email"
              , placeholder = "billygamer5@example.com"
              , name = "email"
              , value = model.signUpEmail.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.emailOk value
              , maxChars = Just 320
              }
            , Utils.myInput (Change SignUpPassword)
              { myInputDefaults
              | labelText = "Password"
              , sublabel = Api.Validate.passwordLabel
              , type_ = "password"
              , placeholder = "hunter2"
              , name = "password"
              , value = model.signUpPassword.value
              , validate = \value ->
                if String.isEmpty value then
                  Just ""
                else
                  Api.Validate.passwordOk value
              , maxChars = Just 200
              }
            , Utils.myInput (Change SignUpPasswordAgain)
              { myInputDefaults
              | labelText = "Password again"
              , type_ = "password"
              , placeholder = "hunter2"
              , name = "password"
              , value = model.signUpPasswordAgain.value
              , validate = \value ->
                if value /= model.signUpPassword.value then
                  Just "Passwords do not match!"
                else
                  Nothing
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

makeConfirmLeave : msg -> msg -> List (Html msg)
makeConfirmLeave onLeave onCancel =
  [ div [ A.class "modal-back show" ]
    [ div [ A.class "modal unsaved-changes" ]
      [ p [ A.class "confirm-msg" ]
        [ text "You have unsaved changes. Are you sure you want to leave?" ]
      , div [ A.class "confirm-btn-wrapper" ]
        [ button [ A.class "button", A.id "confirm-leave-btn", onClick onLeave ]
          [ text "Leave" ]
        , button [ A.class "button cancel-btn", onClick onCancel ]
          [ text "Cancel" ]
        ]
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
      [ Utils.extLink "Github" "https://github.com/Orbiit/elimination" "link"
      , text (" " ++ Utils.char Utils.Middot ++ " ")
      , a [ A.href "?about", A.class "link" ]
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
