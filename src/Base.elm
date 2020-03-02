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
import Utils exposing (Char(..), char)
import Utils.Input as Input exposing (myInputDefaults)
import Utils.HumanTime as HumanTime

type HeaderWindow
  = SignUpWindow
  | LoginWindow
  | Notifications
  | UserOptions
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
  , loginUsername : Input.InputState
  , loginPassword : Input.InputState
  , signUpUsername : Input.InputState
  , signUpName : Input.InputState
  , signUpEmail : Input.InputState
  , signUpPassword : Input.InputState
  , signUpPasswordAgain : Input.InputState
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
    , loginUsername = Input.initInputState
    , loginPassword = Input.initInputState
    , signUpUsername = Input.initInputState
    , signUpName = Input.initInputState
    , signUpEmail = Input.initInputState
    , signUpPassword = Input.initInputState
    , signUpPasswordAgain = Input.initInputState
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
  | Change Input Input.MyInputMsg
  | Login
  | SignUp
  | NewSession AuthMethod String (Api.Response Api.SessionID)
  | SignOut
  | Refresh
  | NotificationsLoaded (Api.Response Api.NotificationResult)
  | LoadMore
  | MoreNotificationsLoaded (Api.Response Api.NotificationResult)
  | MarkAsRead
  | MarkedAsRead (Api.Response ())
  | DoNothing

notifsAtATime : Int
notifsAtATime = 5

updateNotifs : Api.GlobalModel m -> Cmd Msg
updateNotifs global =
  case global.session of
    Api.SignedIn _ ->
      Api.notifications global NotificationsLoaded 0 notifsAtATime
    Api.SignedOut ->
      Cmd.none

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    Open window ->
      ( { model | open = window }
      , Task.attempt (\_ -> DoNothing) <|
        Dom.focus <| case window of
          SignUpWindow -> "sign-up-input"
          LoginWindow -> "login-input"
          Notifications -> if model.notifs.unread == 0 then "refresh-btn" else "read-btn"
          UserOptions -> "my-profile"
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
            { model | loginUsername = Input.updateValue model.loginUsername value valid }
          LoginPassword ->
            { model | loginPassword = Input.updateValue model.loginPassword value valid }
          SignUpUsername ->
            { model | signUpUsername = Input.updateValue model.signUpUsername value valid }
          SignUpName ->
            { model | signUpName = Input.updateValue model.signUpName value valid }
          SignUpEmail ->
            { model | signUpEmail = Input.updateValue model.signUpEmail value valid }
          SignUpPassword ->
            { model | signUpPassword = Input.updateValue model.signUpPassword value valid }
          SignUpPasswordAgain ->
            { model | signUpPasswordAgain = Input.updateValue model.signUpPasswordAgain value valid }
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
              { model | loginLoading = False, loginPassword = Input.initInputState }
            SignUpMethod ->
              { model | signUpLoading = False, signUpPassword = Input.initInputState, signUpPasswordAgain = Input.initInputState }
          , Cmd.none
          , Api.SignIn newSession (String.toLower username)
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
    SignOut ->
      ( model, Cmd.none, Api.AttemptSignOut)
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
      [ text (HumanTime.display zone time) ]
    , case message of
      Api.GameStarted gameID gameName maybeTarget maybeTargetName ->
        case (maybeTarget, maybeTargetName) of
          (Just target, Just targetName) ->
            span [ A.class "notif-msg" ]
              [ a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text " has started! Your target is "
              , a [ A.class "link notif-name", A.href ("?@" ++ target) ]
                [ text targetName ]
              , text "."
              ]
          _ ->
            span [ A.class "notif-msg" ]
              [ a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text " has started! See the home page for your target."
              ]
      Api.GameEnded gameID gameName winner winnerName ->
        span [ A.class "notif-msg" ]
          [ a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text " has ended! "
          , a [ A.class "link notif-name", A.href ("?@" ++ winner) ]
            [ text winnerName ]
          , text " was the last one standing."
          ]
      Api.Killed gameID gameName killer killerName ->
        span [ A.class "notif-msg" ]
          [ a [ A.class "link notif-name", A.href ("?@" ++ killer) ]
            [ text killerName ]
          , text " has just eliminated you in "
          , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text "."
          ]
      Api.KilledSelf gameID gameName maybeTarget maybeTargetName ->
        case (maybeTarget, maybeTargetName) of
          (Just target, Just targetName) ->
            span [ A.class "notif-msg" ]
              [ text "Your target has marked themself as eliminated in "
              , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text ", so your new target is "
              , a [ A.class "link notif-name", A.href ("?@" ++ target) ]
                [ text targetName ]
              , text "."
              ]
          _ ->
            span [ A.class "notif-msg" ]
              [ text "Your target has marked themself as eliminated in "
              , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text ". Check the home page for your new target."
              ]
      Api.Kicked gameID gameName reason ->
        span [ A.class "notif-msg" ]
          [ text "You were kicked from "
          , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text
            (if reason == "" then " for unknown reasons." else " because \"" ++ reason ++ "\".")
          ]
      Api.TargetKicked gameID gameName target targetName ->
        span [ A.class "notif-msg" ]
          [ text "Your target was kicked from "
          , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text ", so your new target is "
          , a [ A.class "link notif-name", A.href ("?@" ++ target) ]
            [ text targetName ]
          , text "."
          ]
      Api.Shuffle gameID gameName maybeTarget maybeTargetName ->
        case (maybeTarget, maybeTargetName) of
          (Just target, Just targetName) ->
            span [ A.class "notif-msg" ]
              [ text "The targets of everyone who is still alive in "
              , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text " have been shuffled. Your new target is "
              , a [ A.class "link notif-name", A.href ("?@" ++ target) ]
                [ text targetName ]
              , text "."
              ]
          _ ->
            span [ A.class "notif-msg" ]
              [ text "The targets of everyone who is still alive in "
              , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
                [ text gameName ]
              , text " have been shuffled. See the home page for your new target."
              ]
      Api.Announcement gameID gameName announcement ->
        span [ A.class "notif-msg" ]
          [ text "An announcement from "
          , a [ A.class "link notif-name", A.href ("?!" ++ gameID) ]
            [ text gameName ]
          , text (": \"" ++ announcement ++ "\"")
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
    , span [ A.class "flex" ] [ text " " ]
    ]
    ++ case session of
      Api.SignedIn { username } ->
        [ if frontPage then
          a [ A.class "button create-game-btn", A.href "?!bbdd6" ]
            [ text "Gunn Elimination 2020" ]
        else
          text ""
        , if frontPage then
          a [ A.class "button create-game-btn", A.href "?create-game" ]
            [ text "Create game" ]
        else
          text ""
        , headerWindow model "icon-btn header-btn notif-btn"
          [ text "Notifications ("
          , span [ A.classList [ ("show-unread", model.notifs.unread > 0) ] ]
            [ text (String.fromInt model.notifs.unread) ]
          , text ")" ]
          Notifications
          [ div [ A.class "header-window notifs" ]
            [ h2 [ A.class "notif-header" ]
              [ text "Notifications"
              , span [ A.class "flex" ] [ text " " ]
              , button
                [ A.class "button small-btn notif-action-btn"
                , A.classList [ ("loading", model.markingAsRead) ]
                , A.id "read-btn"
                , A.disabled (model.markingAsRead || model.notifs.unread == 0)
                , onClick MarkAsRead
                ]
                [ text "Mark as read" ]
              , button
                [ A.class "button small-btn notif-action-btn"
                , A.id "refresh-btn"
                , A.disabled model.notifsLoading
                , onClick Refresh
                ]
                [ text "Refresh" ]
              -- , label [ A.class "email-notifs" ]
              --   [ input [ A.class "checkbox", A.type_ "checkbox" ]
              --     []
              --   , text "Send notifications to my email"
              --   ]
              ]
            , div [] (List.map (renderNotification zone) model.notifs.notifications)
            , case model.notifsProblem of
              Just errorText ->
                span [ A.class "problematic-error" ]
                  [ text errorText ]
              Nothing ->
                text ""
            , if model.notifsLoading then
              button [ A.class "button load-more-btn loading", A.disabled True ]
                [ text "Loading" ]
            else if model.notifs.end then
              text ""
            else
              button [ A.class "button load-more-btn", onClick LoadMore ]
                [ text "Load more" ]
            ]
          ]
        , headerWindow model "header-btn auth-btn username-btn"
          [ span [ A.class "username" ]
            [ text username ]
          , span [ A.class "username-dropdown-arrow" ] []
          ]
          UserOptions
          [ div [ A.class "header-window username-list" ]
            [ a [ A.class "username-item link", A.href ("?@" ++ username), A.id "my-profile", onClick Close ]
              [ text "My profile" ]
            , a [ A.class "username-item link", A.href "?settings", onClick Close ]
              [ text "Account settings" ]
            , div [ A.class "username-divider" ]
              []
            , button [ A.class "username-item link", onClick SignOut ]
              [ text "Sign out" ]
            ]
          ]
        ]
      Api.SignedOut ->
        [ headerWindow model "header-btn auth-btn" [ text "Log in" ] LoginWindow
          [ form [ A.class "header-window", onSubmit Login ]
            [ Input.myInput (Change LoginUsername)
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
              , attributes =
                [ A.attribute "autocapitalize" "none"
                , A.attribute "autocorrect" "off"
                ]
              }
            , Input.myInput (Change LoginPassword)
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
            , span [ A.class "problematic-error" ]
              [ text (Maybe.withDefault "" model.loginProblem) ]
            , p [ A.class "auth-note" ]
              [ text "If you forgot your password, message "
              , Utils.extLink "Ovinus Real" "https://www.facebook.com/ovinus.real" "link"
              , text " or email "
              , a [ A.href "mailto:sy24484@pausd.us", A.class "link" ]
                [ text "mailto:sy24484@pausd.us" ]
              , text " for a password reset link."
              ]
            ]
          ]
        , headerWindow model "header-btn auth-btn" [ text "Sign up" ] SignUpWindow
          [ form [ A.class "header-window", onSubmit SignUp ]
            [ Input.myInput (Change SignUpUsername)
              { myInputDefaults
              | labelText = "Username"
              , sublabel = [ text Api.Validate.usernameLabel ]
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
              , attributes =
                [ A.attribute "autocapitalize" "none"
                , A.attribute "autocorrect" "off"
                ]
              }
            , Input.myInput (Change SignUpName)
              { myInputDefaults
              | labelText = "Full name"
              , sublabel = [ text Api.Validate.nameLabel ]
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
            , Input.myInput (Change SignUpEmail)
              { myInputDefaults
              | labelText = "Email"
              , sublabel = [ text Api.Validate.emailLabel ]
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
            , Input.myInput (Change SignUpPassword)
              { myInputDefaults
              | labelText = "Password"
              , sublabel = [ text Api.Validate.passwordLabel ]
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
            , Input.myInput (Change SignUpPasswordAgain)
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
            , span [ A.class "problematic-error" ]
              [ text (Maybe.withDefault "" model.signUpProblem) ]
            , p [ A.class "auth-note" ]
              [ text "Read our "
              , a [ A.href "?privacy", A.class "link" ]
                [ text "Privacy policy" ]
              , text " to see what we do with these data."
              ]
            ]
          ]
        ]
  ]

makeConfirm : msg -> String -> Html msg -> List (Html msg)
makeConfirm onCancel confirmMsg okBtn =
  [ div [ A.class "modal-back show" ]
    [ div [ A.class "modal unsaved-changes" ]
      [ p [ A.class "confirm-msg" ]
        [ text confirmMsg ]
      , div [ A.class "confirm-btn-wrapper" ]
        [ okBtn
        , button [ A.class "button cancel-btn", onClick onCancel ]
          [ text "Cancel" ]
        ]
      ]
    ]
  ]

makeConfirmLeave : msg -> msg -> List (Html msg)
makeConfirmLeave onLeave onCancel =
  button [ A.class "button", A.id "confirm-leave-btn", onClick onLeave ]
    [ text "Leave" ]
    |> makeConfirm onCancel "You have unsaved changes. Are you sure you want to leave?"

makeConfirmSignOut : msg -> msg -> List (Html msg)
makeConfirmSignOut onSignOut onCancel =
  button [ A.class "button", A.id "confirm-leave-btn", onClick onSignOut ]
    [ text "Sign out" ]
    |> makeConfirm onCancel "You have unsaved changes. Are you sure you want to sign out?"

makeFooter : List (Html msg)
makeFooter =
  [ footer [ A.class "footer" ]
    [ span []
      [ text "From the creators of "
      , Utils.extLink "UGWA" "https://gunn.app/" "link"
      , text "."
      ]
    , span [ A.class "flex" ] [ text " " ]
    , span []
      [ Utils.extLink "Github" "https://github.com/Orbiit/elimination" "link"
      , text (" " ++ char Middot ++ " ")
      , a [ A.href "?about", A.class "link" ]
        [ text "Help" ]
      , text (" " ++ char Middot ++ " ")
      , a [ A.href "?privacy", A.class "link" ]
        [ text "Privacy policy" ]
      , text (" " ++ char Middot ++ " ")
      , a [ A.href "?terms", A.class "link" ]
        [ text "Terms of use" ]
      ]
    ]
  ]
