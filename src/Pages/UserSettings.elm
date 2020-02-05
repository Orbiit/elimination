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
  { values :
    { name : InputState
    , email : InputState
    , bio : InputState
    , password : InputState
    , oldPassword : InputState
    }
  , loadingLogout : Bool
  , loading : Bool
  , problem : Maybe String
  }

init : Api.Session -> Model
init _ =
  { values =
    { name = initInputState
    , email = initInputState
    , bio = initInputState
    , password = initInputState
    , oldPassword = initInputState
    }
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

update : Msg -> Api.Session -> Model -> (Model, Api.PageCmd Msg)
update msg session model =
  case msg of
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
            NameInput ->
              { values | name = updateValue values.name value ok }
            EmailInput ->
              { values | email = updateValue values.email value ok }
            BioInput ->
              { values | bio = updateValue values.bio value ok }
            PasswordInput ->
              { values | password = updateValue values.password value ok }
            OldPasswordInput ->
              { values | oldPassword = updateValue values.oldPassword value ok }
        }, Api.Command Cmd.none)
    Logout ->
      case session of
        Api.SignedIn authSession ->
          ( { model | loadingLogout = True }
          , Api.Command (Api.logout authSession.session LoggedOut)
          )
        Api.SignedOut ->
          (model, Api.Command Cmd.none)
    LoggedOut _ ->
      ({ model | loadingLogout = False }, Api.ChangeSession Api.SignedOut)
    InfoLoaded result ->
      case result of
        Ok { name, email, bio } ->
          let
            values = model.values
          in
            ( { model
              | values =
                { values
                | name = inputState name
                , email = inputState email
                , bio = inputState bio
                }
              }
            , Api.ChangePage Pages.UserSettings (NProgress.done ())
            )
        Err error ->
          (model, Api.ChangePage (Pages.Error error) (NProgress.done ()))
    Save ->
      case session of
        Api.SignedIn authSession ->
          ( { model | loading = True, problem = Nothing }
          , Api.Command <|
            Api.setSettings (E.object
              (List.filterMap (\a -> a)
                [ if model.values.name.value /= model.values.name.original then
                  Just ("name", E.string model.values.name.value)
                else
                  Nothing
                , if model.values.email.value /= model.values.email.original then
                  Just ("email", E.string model.values.email.value)
                else
                  Nothing
                , if model.values.bio.value /= model.values.bio.original then
                  Just ("bio", E.string model.values.bio.value)
                else
                  Nothing
                , if String.isEmpty model.values.password.value then
                  Nothing
                else
                  Just ("password", E.string model.values.password.value)
                -- Using password value here so that oldPassword can be empty
                , if String.isEmpty model.values.password.value then
                  Nothing
                else
                  Just ("oldPassword", E.string model.values.oldPassword.value)
                ]
              )
            ) authSession.session Saved
          )
        Api.SignedOut ->
          (model, Api.Command Cmd.none)
    Saved result ->
      case result of
        Ok _ ->
          let
            values = model.values
          in
            ( { model
              | loading = False
              , values =
                { values
                | name = inputState model.values.name.value
                , email = inputState model.values.email.value
                , bio = inputState model.values.bio.value
                , password = initInputState
                , oldPassword = initInputState
                }
              }
            , Api.Command Cmd.none
            )
        Err error ->
          ({ model | loading = False, problem = Just (Tuple.second error) }, Api.Command Cmd.none)

view : Api.Session -> Model -> List (Html Msg)
view session model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      [ text "User settings"
      , span [ A.class "flex" ]
        []
      , a [ A.class "button", A.href "?user" ]
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
          , value = model.values.name.value
          , validate = Api.Validate.nameOk
          , maxChars = Nothing
          , storeValueMsg = Change NameInput }
        , Utils.myInput
          { labelText = "Email"
          , sublabel = Api.Validate.emailLabel
          , type_ = "email"
          , placeholder = "billygamer5@example.com"
          , value = model.values.email.value
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
          , value = model.values.bio.value
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
          , value = model.values.password.value
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
          , value = model.values.oldPassword.value
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change OldPasswordInput }
        ]
      , let
          valid = model.values.name.valid &&
            model.values.email.valid &&
            model.values.bio.valid &&
            model.values.password.valid &&
            model.values.oldPassword.valid
          changed = model.values.name.value /= model.values.name.original ||
            model.values.email.value /= model.values.email.original ||
            model.values.bio.value /= model.values.bio.original ||
            not (String.isEmpty model.values.password.value)
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
