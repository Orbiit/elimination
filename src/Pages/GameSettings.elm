module Pages.GameSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onSubmit, stopPropagationOn, onClick)
import Json.Decode as D
import Json.Encode as E
import Time

import Utils exposing (char, Char(..))
import Api
import Pages
import NProgress

type Input
  = NameInput
  | DescInput
  | PasswordInput
  | ReasonInput

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
  , modal : Maybe String
  , kickReason : String
  , kickProblem : Maybe String
  , kicking : Bool
  }

init : Model
init =
  { game = Nothing
  , name = Utils.updateValue Utils.initInputState "" False
  , desc = Utils.initInputState
  , password = Utils.initInputState
  , players = []
  , started = False
  , ended = False
  , loading = False
  , problem = Nothing
  , modal = Nothing
  , kickReason = ""
  , kickProblem = Nothing
  , kicking = False
  }

type Msg
  = Change Input (String -> Maybe String) String
  | InfoLoaded Api.GameID (Result Utils.HttpError Api.GameSettingsInfo)
  | Save
  | Saved (Result Utils.HttpError Api.GameID)
  | ShowModal String
  | HideModal
  | DontClose
  | Kick
  | Kicked String (Result Utils.HttpError ())

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg { session } model =
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
          ReasonInput ->
            { model | kickReason = value }
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
          , if model.game == Nothing then
            Api.Redirect ("?settings!" ++ gameID)
          else
            Api.None
          )
        Err error ->
          ({ model | loading = False, problem = Just (Tuple.second error) }, Cmd.none, Api.sessionCouldExpire error)
    ShowModal username ->
      ({ model | modal = Just username, kickReason = "", kickProblem = Nothing }, Cmd.none, Api.None)
    HideModal ->
      ({ model | modal = Nothing }, Cmd.none, Api.None)
    DontClose ->
      (model, Cmd.none, Api.None)
    Kick ->
      case (model.game, model.modal, session) of
        (Just gameID, Just username, Api.SignedIn authSession) ->
          ( { model | kicking = True, kickProblem = Nothing }
          , Api.kick username model.kickReason gameID authSession.session (Kicked username)
          , Api.None
          )
        _ ->
          (model, Cmd.none, Api.None)
    Kicked username result ->
      case result of
        Ok _ ->
          ( { model
            | kicking = False
            , modal = Nothing
            , players = List.filter (\player -> player.username /= username) model.players
            }
          , Cmd.none
          , Api.None
          )
        Err ((_, errorMsg) as error) ->
          ({ model | kicking = False, kickProblem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)

renderPlayer : Model -> Time.Zone -> Api.GameSettingsPlayer -> Html Msg
renderPlayer model zone player =
  div [ A.class "member-item" ]
    ([ div [ A.class "member" ]
      [ a [ A.class "link member-link", A.href ("?@" ++ player.username) ]
        [ text player.name ]
      , span [ A.class "member-info" ]
        [ text ("Joined " ++ Utils.displayTime zone player.joined ++ " "
          ++ char Middot ++ " " ++ String.fromInt player.kills
          ++ (if player.kills == 1 then " elimination" else " eliminations"))
        ]
      ]
    , button [ A.class "button kick-btn", onClick (ShowModal player.username) ]
      [ text "Kick" ]
    ]
    ++ case model.modal of
      Just username ->
        if username == player.username then
          [ div [ A.class "modal-back show", onClick HideModal ]
            [ form
              [ A.class "modal join-modal"
              , stopPropagationOn "click" (D.succeed (DontClose, True))
              , onSubmit Kick
              ]
              ([ Utils.myInput
                { labelText = "Kick reason (optional)"
                , sublabel = ""
                , type_ = "text"
                , placeholder = "Undesirable."
                , value = model.kickReason
                , validate = \value -> Nothing
                , maxChars = Nothing
                , storeValueMsg = Change ReasonInput }
              , input
                [ A.class "button submit-btn"
                , A.classList [ ("loading", model.kicking) ]
                , A.type_ "submit"
                , A.value "Kick"
                , A.disabled model.kicking
                ]
                []
              ]
              ++ case model.kickProblem of
                Just errorText ->
                  [ span [ A.class "problematic-error" ]
                    [ text errorText ] ]
                Nothing ->
                  [])
            ] ]
        else
          []
      Nothing ->
        [])

view : Api.GlobalModel m -> Model -> List (Html Msg)
view { zone } model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      (Utils.filter
        [ Just (text "Game settings")
        , Just (span [ A.class "flex" ] [])
        , if model.game /= Nothing && not model.started then
          Just (button
            [ A.class "button"
            , A.disabled (List.length model.players < 2)
            ]
            [ text "Start game" ])
        else
          Nothing
        , case model.game of
          Just gameID ->
            Just (a [ A.class "button", A.href ("?!" ++ gameID) ]
              [ text "View game page" ])
          Nothing ->
            Nothing
        ])
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
      ((h2 [ A.class "members-header" ]
        ([ text ("Participants (" ++ String.fromInt (List.length model.players) ++ ")")
        , span [ A.class "flex" ]
          []
        ]
        ++ (if model.started then
          [ button [ A.class "button" ]
            [ text "Shuffle targets" ] ]
        else
          [])))
      :: List.map (renderPlayer model zone) model.players
      )
    ]
  ]
