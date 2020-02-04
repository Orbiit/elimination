module Pages.UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)

import Api
import Utils
import Pages

type Input
  = NameInput
  | EmailInput
  | BioInput
  | PasswordInput
  | OldPasswordInput

type alias Model =
  { values :
    { name : (String, Bool)
    , email : (String, Bool)
    , bio : (String, Bool)
    , password : (String, Bool)
    , oldPassword : (String, Bool)
    }
  , info : Maybe Api.UserSettingsInfo
  }

init : Api.Session -> Model
init _ =
  { values =
    { name = ("", False)
    , email = ("", False)
    , bio = ("", False)
    , password = ("", False)
    , oldPassword = ("", False)
    }
  , info = Nothing
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
              { values | name = (value, ok) }
            EmailInput ->
              { values | email = (value, ok) }
            BioInput ->
              { values | bio = (value, ok) }
            PasswordInput ->
              { values | password = (value, ok) }
            OldPasswordInput ->
              { values | oldPassword = (value, ok) }
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
        Ok userInfo ->
          ({ model | info = Just userInfo }, Api.ChangePage Pages.UserSettings)
        Err error ->
          (model, Api.ChangePage <| Pages.Error error)

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
          , value = Tuple.first model.values.name
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change NameInput }
        , Utils.myInput
          { labelText = "Email"
          , sublabel = "We will send password reset forms (and notifications if enabled) to this email."
          , type_ = "email"
          , placeholder = "billygamer5@example.com"
          , value = Tuple.first model.values.email
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
          , value = Tuple.first model.values.bio
          , validate = \value -> Nothing
          , maxChars = Nothing
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
          , value = Tuple.first model.values.password
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change PasswordInput }
        , Utils.myInput
          { labelText = "Old password"
          , sublabel = ""
          , type_ = "password"
          , placeholder = "hunter2"
          , value = Tuple.first model.values.oldPassword
          , validate = \value -> Nothing
          , maxChars = Nothing
          , storeValueMsg = Change OldPasswordInput }
        ]
      , input [ A.class "button submit-btn", A.disabled True, A.type_ "submit", A.value "Save" ]
        []
      ]
    ]
  ]
