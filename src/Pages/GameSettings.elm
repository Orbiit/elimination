module Pages.GameSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onSubmit)
import Json.Encode as E

import Utils
import Api
import Pages
import NProgress

type Input
  = NameInput
  | DescInput
  | PasswordInput

type alias Model =
  { game : Maybe Api.GameID
  , name : Utils.InputState
  , desc : Utils.InputState
  , password : Utils.InputState
  , players : List Api.GameSettingsPlayer
  , started : Bool
  , ended : Bool
  , loading : Bool
  , problem : Maybe String
  }

reset : Model
reset =
  { game = Nothing
  , name = Utils.updateValue Utils.initInputState "" False
  , desc = Utils.initInputState
  , password = Utils.initInputState
  , players = []
  , started = False
  , ended = False
  , loading = False
  , problem = Nothing
  }

init : Api.Session -> Model
init _ = reset

type Msg
  = Change Input (String -> Maybe String) String
  | InfoLoaded Api.GameID (Result Utils.HttpError Api.GameSettingsInfo)
  | Save
  | Saved (Result Utils.HttpError Api.GameID)

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
            { model | name = Utils.updateValue model.name value ok }
          DescInput ->
            { model | desc = Utils.updateValue model.desc value ok }
          PasswordInput ->
            { model | password = Utils.updateValue model.password value ok }
        , Cmd.none
        , Api.None
        )
    InfoLoaded game result ->
      case result of
        Ok { name, description, password, players, started, ended } ->
          ( { model
            | game = Just game
            , name = Utils.inputState name
            , desc = Utils.inputState description
            , password = Utils.inputState password
            , players = players
            , started = started
            , ended = ended
            }
          , NProgress.done ()
          , Api.ChangePage Pages.GameSettings
          )
        Err error ->
          (model, NProgress.done (), Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ])
    Save ->
      case session of
        Api.SignedIn authSession ->
          let
            creating = model.game == Nothing
            gameInfo =
              (E.object
                (Utils.filter
                  [ if creating || model.name.value /= model.name.original then
                    Just ("name", E.string model.name.value)
                  else
                    Nothing
                  , if creating || model.desc.value /= model.desc.original then
                    Just ("description", E.string model.desc.value)
                  else
                    Nothing
                  , if creating || model.password.value /= model.password.original then
                    Just ("password", E.string model.password.value)
                  else
                    Nothing
                  ]
                )
              )
          in
            ( { model | loading = True, problem = Nothing }
            , case model.game of
              Just gameID ->
                Api.setGameSettings gameInfo gameID authSession.session Saved
              Nothing ->
                Api.createGame gameInfo authSession.session Saved
            , Api.None
            )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Saved result ->
      case result of
        Ok gameID ->
          ( { model
            | loading = False
            , game = Just gameID
            , name = Utils.inputState model.name.value
            , desc = Utils.inputState model.desc.value
            , password = Utils.inputState model.password.value
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
      [ text "Game settings"
      , span [ A.class "flex" ]
        []
      , button [ A.class "button", A.attribute "disabled" "" ]
        [ text "Start game" ]
      , a [ A.class "button", A.href "./game.html" ]
        [ text "View game page" ]
      ]
    , form [ onSubmit Save ]
      ([ div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "Name"
          , sublabel = "Required."
          , type_ = "text"
          , placeholder = "The People's Elimination Game"
          , value = model.name.value
          , validate = \value ->
            if String.isEmpty value then
              Just (if model.game == Nothing then "" else "The name cannot be empty.")
            else if String.length value > 100 then
              Just "The name cannot be over 100 characters long."
            else
              Nothing
          , maxChars = Just 100
          , storeValueMsg = Change NameInput }
        , Utils.myInput
          { labelText = "Passphrase to join"
          , sublabel = "Share this passphrase to people who you want to join."
          , type_ = "text"
          , placeholder = "hunter2"
          , value = model.password.value
          , validate = \value ->
            if String.length value > 200 then
              Just "The passphrase cannot be over 200 characters long."
            else
              Nothing
          , maxChars = Just 200
          , storeValueMsg = Change PasswordInput }
        ]
      , div [ A.class "input-row" ]
        [ Utils.myInput
          { labelText = "Description and rules"
          , sublabel = "List rules for elimination here, such as how they can be blocked, and when and where eliminations are allowed to be made."
          , type_ = "textarea"
          , placeholder = "Please pick up the bowling balls from the front office by February 30th. Eliminations may only occur when the student is not carrying their bowling ball. Eliminations may not occur inside classes. Inappropriate behaviour will result in immediate disqualification. Targets will be shuffled every week. Good luck, Pizzas!"
          , value = model.desc.value
          , validate = \value ->
            if String.length value > 2000 then
              Just "Description can only be at most 2000 characters long."
            else
              Nothing
          , maxChars = Just 2000
          , storeValueMsg = Change DescInput }
        ]
      , let
          valid = model.name.valid &&
            model.desc.valid &&
            model.password.valid
          changed = model.name.value /= model.name.original ||
            model.desc.value /= model.desc.original ||
            model.password.value /= model.password.original
        in
          input
            [ A.class "button submit-btn"
            , A.classList [ ("loading", model.loading) ]
            , A.type_ "submit"
            , A.value (if model.game == Nothing then "Create" else if changed then "Save" else "Saved")
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
    , div [ A.class "members" ]
      [ h2 [ A.class "members-header" ]
        [ text "Participants (6)"
        , span [ A.class "flex" ]
          []
        , button [ A.class "button" ]
          [ text "Shuffle targets" ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Ern Seth" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-23 · 0 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Sergo Neristo" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-23 · 1 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Jame Sooth" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-22 · 2 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Eghten Siuvoulet" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-21 · 0 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Zuck Ergostan" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-19 · 0 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      , div [ A.class "member-item" ]
        [ div [ A.class "member" ]
          [ a [ A.class "link member-link", A.href "./user.html" ]
            [ text "Memphie Ratch" ]
          , span [ A.class "member-info" ]
            [ text "Joined 2020-01-15 · 0 eliminations" ]
          ]
        , button [ A.class "button kick-btn" ]
          [ text "Kick" ]
        , div [ A.class "modal-back" ]
          [ form [ A.class "modal join-modal" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Kick reason (optional)" ]
              , div [ A.class "input" ]
                [ input [ A.name "reason", A.placeholder "Undesirable.", A.type_ "text" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Kick" ]
              []
            ]
          ]
        ]
      ]
    ]
  ]
