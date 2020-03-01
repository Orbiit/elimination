module Pages.ResetPassword exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onSubmit)

import Api
import Api.Validate
import Pages
import Utils.Input as Input exposing (myInputDefaults)

type alias Model =
  { password : Input.InputState
  , passwordAgain : Input.InputState
  , loading : Bool
  , problem : Maybe String
  }

init : Model
init =
  { password = Input.initInputState
  , passwordAgain = Input.initInputState
  , loading = False
  , problem = Nothing
  }

type Input
  = Password
  | PasswordAgain

type Msg
  = Change Input Input.MyInputMsg
  | Submit
  | Reset (Api.Response (String, String))

update : Msg -> Api.GlobalModel m -> Model -> String -> (Model, Cmd Msg, Api.PageCmd)
update msg global model id =
  case msg of
    Change input { validate, value, scrollHeight } ->
      let
        ok = validate value == Nothing
      in
        ( case input of
          Password ->
            { model | password = Input.updateValue model.password value ok }
          PasswordAgain ->
            { model | passwordAgain = Input.updateValue model.passwordAgain value ok }
        , Cmd.none
        , Api.None
        )
    Submit ->
      ( { model | loading = True, problem = Nothing }
      , Api.resetPassword global Reset id model.password.value
      , Api.None
      )
    Reset result ->
      case result of
        Ok (session, username) ->
          ( { model
            | loading = False
            , password = Input.initInputState
            , passwordAgain = Input.initInputState
            }
          , Cmd.none
          , Api.Batch
            [ Api.Redirect "?"
            , Api.SignIn session username
            ]
          )
        Err (_, error) ->
          ({ model | loading = False, problem = Just error }, Cmd.none, Api.None)

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  [ form [ A.class "main content text", onSubmit Submit ]
    [ h1 []
      [ text "Reset your password" ]
    , Input.myInput (Change Password)
      { myInputDefaults
      | labelText = "Password"
      , sublabel = [ text Api.Validate.passwordLabel ]
      , type_ = "password"
      , placeholder = "hunter2"
      , name = "password"
      , value = model.password.value
      , validate = \value ->
        if String.isEmpty value then
          Just ""
        else
          Api.Validate.passwordOk value
      , maxChars = Just 200
      }
    , Input.myInput (Change PasswordAgain)
      { myInputDefaults
      | labelText = "Password again"
      , type_ = "password"
      , placeholder = "hunter2"
      , name = "password"
      , value = model.passwordAgain.value
      , validate = \value ->
        if value /= model.password.value then
          Just "Passwords do not match!"
        else
          Nothing
      }
    , input
      [ A.class "button submit-btn"
      , A.classList [ ("loading", model.loading) ]
      , A.type_ "submit"
      , A.value "Reset"
      , A.disabled (model.loading || not (model.password.valid && model.passwordAgain.valid))
      ]
      []
    , span [ A.class "problematic-error" ]
      [ text (Maybe.withDefault "" model.problem) ]
    ]
  ]
