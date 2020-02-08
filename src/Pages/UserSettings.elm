module Pages.UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onSubmit)
import Json.Encode as E

import Api
import Api.Validate
import Utils
import Pages
import NProgress

type Input
  = NameInput
  | EmailInput
  | BioInput
  | PasswordInput
  | OldPasswordInput

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

type alias Model =
  { name : InputState
  , email : InputState
  , bio : InputState
  , password : InputState
  , oldPassword : InputState
  , loadingLogout : Bool
  , loading : Bool
  , problem : Maybe String
  }

init : Api.Session -> Model
init _ =
  { name = initInputState
  , email = initInputState
  , bio = initInputState
  , password = initInputState
  , oldPassword = initInputState
  , loadingLogout = False
  , loading = False
  , problem = Nothing
  }

type Msg
  = Change Input (String -> Maybe String) String
  | Logout
  | LoggedOut (Result Utils.HttpError ())
  | InfoLoaded (Result Utils.HttpError Api.UserSettingsInfo)
  | Save
  | Saved (Result Utils.HttpError ())

update : Msg -> Api.Session -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg session model =
  case msg of
    Change input validate value ->
      let
        ok = case validate value of
          Just _ ->
            False
          Nothing ->
            True
      in
        ( case input of
          NameInput ->
            { model | name = updateValue model.name value ok }
          EmailInput ->
            { model | email = updateValue model.email value ok }
          BioInput ->
            { model | bio = updateValue model.bio value ok }
          PasswordInput ->
            { model | password = updateValue model.password value ok }
          OldPasswordInput ->
            { model | oldPassword = updateValue model.oldPassword value ok }
        , Cmd.none
        , Api.None
        )
    Logout ->
      case session of
        Api.SignedIn authSession ->
          ( { model | loadingLogout = True }
          , Api.logout authSession.session LoggedOut
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    LoggedOut _ ->
      ({ model | loadingLogout = False }, Cmd.none, Api.ChangeSession Api.SignedOut)
    InfoLoaded result ->
      case result of
        Ok { name, email, bio } ->
          ( { model
            | name = inputState name
            , email = inputState email
            , bio = inputState bio
            }
          , NProgress.done ()
          , Api.ChangePage Pages.UserSettings
          )
        Err error ->
          (model, NProgress.done (), Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ])
    Save ->
      case session of
        Api.SignedIn authSession ->
          ( { model | loading = True, problem = Nothing }
          , Api.setSettings (E.object
              (List.filterMap (\a -> a)
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
            ) authSession.session Saved
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Saved result ->
      case result of
        Ok _ ->
          ( { model
            | loading = False
            , name = inputState model.name.value
            , email = inputState model.email.value
            , bio = inputState model.bio.value
            , password = initInputState
            , oldPassword = initInputState
            }
          , Cmd.none
          , Api.None
          )
        Err error ->
          ({ model | loading = False, problem = Just (Tuple.second error) }, Cmd.none, Api.sessionCouldExpire error)

view : Api.Session -> Model -> List (Html Msg)
view session model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      [ text "User settings"
      , span [ A.class "flex" ]
        []
      , a
        [ A.class "button"
        , A.href <|
          case session of
            Api.SignedIn authSession ->
              "?@" ++ authSession.username
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
      ([ div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "Display name"
          , sublabel = Api.Validate.nameLabel
          , type_ = "text"
          , placeholder = "Billy Chelontuvier"
          , value = model.name.value
          , validate = Api.Validate.nameOk
          , maxChars = Nothing
          , storeValueMsg = Change NameInput }
        , Utils.myInput
          { labelText = "Email"
          , sublabel = Api.Validate.emailLabel
          , type_ = "email"
          , placeholder = "billygamer5@example.com"
          , value = model.email.value
          , validate = Api.Validate.emailOk
          , maxChars = Nothing
          , storeValueMsg = Change EmailInput }
        ]
      , div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "Bio"
          , sublabel = ""
          , type_ = "textarea"
          , placeholder = "Introduce yourself here"
          , value = model.bio.value
          , validate = \value ->
            if String.length value > 2000 then
              Just "Bio can only be at most 2000 characters long."
            else
              Nothing
          , maxChars = Just 2000
          , storeValueMsg = Change BioInput }
        ]
      , h2 []
        [ text "Change your password" ]
      , div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "New password"
          , sublabel = Api.Validate.passwordLabel
          , type_ = "password"
          , placeholder = "hunter2"
          , value = model.password.value
          , validate = \value ->
            if String.isEmpty value then
              Nothing
            else
              Api.Validate.passwordOk value
          , maxChars = Nothing
          , storeValueMsg = Change PasswordInput }
        , Utils.myInput
          { labelText = "Old password"
          , sublabel = ""
          , type_ = "password"
          , placeholder = "hunter2"
          , value = model.oldPassword.value
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change OldPasswordInput }
        ]
      , let
          valid = model.name.valid &&
            model.email.valid &&
            model.bio.valid &&
            model.password.valid &&
            model.oldPassword.valid
          changed = model.name.value /= model.name.original ||
            model.email.value /= model.email.original ||
            model.bio.value /= model.bio.original ||
            not (String.isEmpty model.password.value)
        in
          input
            [ A.class "button submit-btn"
            , A.classList [ ("loading", model.loading) ]
            , A.type_ "submit"
            , A.value (if changed then "Save" else "Saved")
            , A.disabled <| model.loading || not changed || not valid
            ]
            []
      ]
      ++ case model.problem of
        Just errorText ->
          [ span [ A.class "problematic-error" ]
            [ text errorText ] ]
        Nothing ->
          [])
    ]
  ]
