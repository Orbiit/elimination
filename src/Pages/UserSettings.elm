module Pages.UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onSubmit)
import Json.Encode as E
import Browser.Dom as Dom
import Task

import Api
import Api.Validate
import Utils
import Utils.Input as Input exposing (myInputDefaults)
import Pages
import NProgress

type Input
  = NameInput
  | EmailInput
  | BioInput
  | PasswordInput
  | OldPasswordInput

type alias Model =
  { name : Input.InputState
  , email : Input.InputState
  , bio : Input.InputState
  , bioHeight : Float
  , password : Input.InputState
  , oldPassword : Input.InputState
  , loadingLogout : Bool
  , loading : Bool
  , problem : Maybe String
  }

init : Model
init =
  { name = Input.initInputState
  , email = Input.initInputState
  , bio = Input.initInputState
  , bioHeight = 0
  , password = Input.inputState ""
  , oldPassword = Input.inputState ""
  , loadingLogout = False
  , loading = False
  , problem = Nothing
  }

type Msg
  = Change Input Input.MyInputMsg
  | ResizeBio (Result Dom.Error Dom.Viewport)
  | Logout
  | LoggedOut (Api.Response ())
  | InfoLoaded (Api.Response Api.UserSettingsInfo)
  | Save
  | Saved (Api.Response ())

resizeBio : Cmd Msg
resizeBio =
  Task.attempt ResizeBio (Dom.getViewportOf "user-bio")

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    Change input { validate, value, scrollHeight } ->
      let
        ok = case validate value of
          Just _ ->
            False
          Nothing ->
            True
      in
        ( case input of
          NameInput ->
            { model | name = Input.updateValue model.name value ok }
          EmailInput ->
            { model | email = Input.updateValue model.email value ok }
          BioInput ->
            { model | bio = Input.updateValue model.bio value ok, bioHeight = scrollHeight }
          PasswordInput ->
            { model | password = Input.updateValue model.password value ok }
          OldPasswordInput ->
            { model | oldPassword = Input.updateValue model.oldPassword value ok }
        , Cmd.none
        , Api.None
        )
    ResizeBio result ->
      case result of
        Ok viewport ->
          ({ model | bioHeight = viewport.scene.height }, Cmd.none, Api.None)
        Err _ ->
          (model, Cmd.none, Api.None)
    Logout ->
      ( { model | loadingLogout = True }, Api.logout global LoggedOut, Api.None)
    LoggedOut _ ->
      ( { model | loadingLogout = False }
      , Cmd.none
      , Api.Batch
        [ Api.ChangeSession Api.SignedOut
        , Api.Redirect "?"
        ]
      )
    InfoLoaded result ->
      case result of
        Ok { name, email, bio } ->
          ( { model
            | name = Input.inputState name
            , email = Input.inputState email
            , bio = Input.inputState bio
            , bioHeight = 0
            }
          , Cmd.batch [ NProgress.done (), resizeBio ]
          , Api.ChangePage Pages.UserSettings
          )
        Err error ->
          (model, NProgress.done (), Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ])
    Save ->
      ( { model | loading = True, problem = Nothing }
      , Api.setSettings global Saved (E.object
          (Utils.filter
            [ if model.name.value /= model.name.original then
              Just ("name", E.string model.name.value)
            else
              Nothing
            , if model.email.value /= model.email.original then
              Just ("email", E.string model.email.value)
            else
              Nothing
            , if model.bio.value /= model.bio.original then
              Just ("bio", E.string model.bio.value)
            else
              Nothing
            , if String.isEmpty model.password.value then
              Nothing
            else
              Just ("password", E.string model.password.value)
            -- Using password value here so that oldPassword can be empty
            , if String.isEmpty model.password.value then
              Nothing
            else
              Just ("oldPassword", E.string model.oldPassword.value)
            ]
          )
        )
      , Api.None
      )
    Saved result ->
      case result of
        Ok _ ->
          ( { model
            | loading = False
            , name = Input.inputState model.name.value
            , email = Input.inputState model.email.value
            , bio = Input.inputState model.bio.value
            , password = Input.inputState ""
            , oldPassword = Input.inputState ""
            }
          , Cmd.none
          , Api.None
          )
        Err error ->
          ({ model | loading = False, problem = Just (Tuple.second error) }, Cmd.none, Api.sessionCouldExpire error)

discardChanges : Model -> Model
discardChanges model =
  { model
  | name = Input.initInputState
  , email = Input.initInputState
  , bio = Input.initInputState
  , bioHeight = 0
  , password = Input.inputState ""
  , oldPassword = Input.inputState ""
  }

hasUnsavedChanges : Model -> Bool
hasUnsavedChanges model =
  model.name.value /= model.name.original ||
    model.email.value /= model.email.original ||
    model.bio.value /= model.bio.original ||
    not (String.isEmpty model.password.value)

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      [ span []
        [ text "User settings" ]
      , span [ A.class "flex" ] [ text " " ]
      , a
        [ A.class "button"
        , A.href <|
          case global.session of
            Api.SignedIn { username } ->
              "?@" ++ username
            Api.SignedOut ->
              "?"
        ]
        [ text "View profile" ]
      , button
        [ A.class "button"
        , A.classList [ ("loading", model.loadingLogout) ]
        , onClick Logout
        , A.disabled model.loadingLogout
        ]
        [ text "Sign out" ]
      ]
    , form [ onSubmit Save ]
      [ div [ A.class "input-row" ]
        [ Input.myInput (Change NameInput)
          { myInputDefaults
          | labelText = "Display name"
          , sublabel = [ text Api.Validate.nameLabel ]
          , placeholder = "Billy Chelontuvier"
          , value = model.name.value
          , validate = Api.Validate.nameOk
          , maxChars = Just 50
          }
        , Input.myInput (Change EmailInput)
          { myInputDefaults
          | labelText = "Email"
          , sublabel = [ text Api.Validate.emailLabel ]
          , type_ = "email"
          , placeholder = "billygamer5@example.com"
          , value = model.email.value
          , validate = Api.Validate.emailOk
          , maxChars = Just 320
          }
        ]
      , div [ A.class "input-row" ]
        [ Input.myInput (Change BioInput)
          { myInputDefaults
          | labelText = "Bio"
          , sublabel =
            [ a [ A.class "link", A.href "?about#formatting" ]
              [ text "Basic formatting" ]
            , text " is available."
            ]
          , type_ = "textarea"
          , placeholder = "Introduce yourself here"
          , value = model.bio.value
          , validate = \value ->
            if String.length value > 2000 then
              Just "Bio can only be at most 2000 characters long."
            else
              Nothing
          , maxChars = Just 2000
          , id = Just "user-bio"
          , height = Just (String.fromFloat (model.bioHeight + 6) ++ "px")
          }
        ]
      , h2 []
        [ text "Change your password" ]
      , div [ A.class "input-row" ]
        [ Input.myInput (Change PasswordInput)
          { myInputDefaults
          | labelText = "New password"
          , sublabel = [ text Api.Validate.passwordLabel ]
          , type_ = "password"
          , placeholder = "hunter2"
          , value = model.password.value
          , validate = \value ->
            if String.isEmpty value then
              Nothing
            else
              Api.Validate.passwordOk value
          , maxChars = Just 200
          }
        , Input.myInput (Change OldPasswordInput)
          { myInputDefaults
          | labelText = "Old password"
          , type_ = "password"
          , placeholder = "hunter2"
          , value = model.oldPassword.value
          }
        ]
      , let
          valid = model.name.valid &&
            model.email.valid &&
            model.bio.valid &&
            model.password.valid &&
            model.oldPassword.valid
          changed = hasUnsavedChanges model
        in
          input
            [ A.class "button submit-btn"
            , A.classList [ ("loading", model.loading) ]
            , A.type_ "submit"
            , A.value (if changed then "Save" else "Saved")
            , A.disabled <| model.loading || not changed || not valid
            ]
            []
      , case model.problem of
        Just errorText ->
          span [ A.class "problematic-error" ]
            [ text errorText ]
        Nothing ->
          text ""
      ]
    ]
  ]
