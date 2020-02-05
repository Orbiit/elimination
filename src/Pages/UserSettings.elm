module Pages.UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)

import Api
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
  }

type Msg
  = Change Input (String -> Maybe String) String
  | Logout
  | LoggedOut (Result Utils.HttpError ())
  | InfoLoaded (Result Utils.HttpError Api.UserSettingsInfo)

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
          (model, Api.Command (Api.logout authSession.session LoggedOut))
        Api.SignedOut ->
          (model, Api.Command Cmd.none)
    LoggedOut _ ->
      (model, Api.ChangeSession Api.SignedOut)
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

view : Api.Session -> Model -> List (Html Msg)
view session model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      [ text "User settings"
      , span [ A.class "flex" ]
        []
      , a [ A.class "button", A.href "?user" ]
        [ text "View profile" ]
      , button [ A.class "button", onClick Logout ]
        [ text "Sign out" ]
      ]
    , form []
      [ div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "Display name"
          , sublabel = "This lets others be able to find and eliminate you, which makes the game fair."
          , type_ = "text"
          , placeholder = "Billy Chelontuvier"
          , value = model.values.name.value
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change NameInput }
        , Utils.myInput
          { labelText = "Email"
          , sublabel = "We will send password reset forms (and notifications if enabled) to this email."
          , type_ = "email"
          , placeholder = "billygamer5@example.com"
          , value = model.values.email.value
          , validate = \value -> Nothing
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
          , validate = \value -> Nothing
          , maxChars = Just 2000
          , storeValueMsg = Change BioInput }
        ]
      , h2 []
        [ text "Change your password" ]
      , div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "New password"
          , sublabel = "Must be at least 3 poop emoji long."
          , type_ = "password"
          , placeholder = "hunter2"
          , value = model.values.password.value
          , validate = \value -> Nothing
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
      , input
        [ A.class "button submit-btn"
        , A.type_ "submit"
        , A.value "Save"
        , A.disabled <| not <|
          model.values.name.valid &&
          model.values.email.valid &&
          model.values.bio.valid &&
          model.values.password.valid &&
          model.values.oldPassword.valid &&
          ( model.values.name.value /= model.values.name.original
          || model.values.email.value /= model.values.email.original
          || model.values.bio.value /= model.values.bio.original
          )
        ]
        []
      ]
    ]
  ]
