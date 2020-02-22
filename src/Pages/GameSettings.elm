module Pages.GameSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onSubmit, stopPropagationOn, onClick)
import Json.Decode as D
import Json.Encode as E
import Time
import Browser.Dom as Dom
import Task

import Utils exposing (char, Char(..))
import Utils.Input as Input exposing (myInputDefaults)
import Utils.HumanTime as HumanTime
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
  -- Game info
  , name : Input.InputState
  , desc : Input.InputState
  , descHeight : Float
  , password : Input.InputState
  , players : List Api.GameSettingsPlayer
  , state : Api.GameState
  -- Page state
  , loading : Bool
  , problem : Maybe String
  , modal : Maybe String
  , kickReason : String
  , kickProblem : Maybe String
  , kicking : Bool
  , starting : Bool
  , shuffling : Bool
  , shuffled : Bool
  }

init : Model
init =
  { game = Nothing
  , name = Input.initInputState
  , desc = Input.updateValue Input.initInputState "" True
  , descHeight = 0
  , password = Input.updateValue Input.initInputState "" True
  , players = []
  , state = Api.WillStart
  , loading = False
  , problem = Nothing
  , modal = Nothing
  , kickReason = ""
  , kickProblem = Nothing
  , kicking = False
  , starting = False
  , shuffling = False
  , shuffled = False
  }

type Msg
  = Change Input Input.MyInputMsg
  | ResizeDesc (Result Dom.Error Dom.Viewport)
  | InfoLoaded Api.GameID (Api.Response Api.GameSettingsInfo)
  | Save
  | Saved (Api.Response Api.GameID)
  | ShowModal String
  | HideModal
  | Kick
  | Kicked String (Api.Response ())
  | Start
  | Started (Api.Response ())
  | Shuffle
  | Shuffled (Api.Response ())
  | DoNothing

resizeDesc : Cmd Msg
resizeDesc =
  Task.attempt ResizeDesc (Dom.getViewportOf "game-desc")

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
          DescInput ->
            { model | desc = Input.updateValue model.desc value ok, descHeight = scrollHeight }
          PasswordInput ->
            { model | password = Input.updateValue model.password value ok }
          ReasonInput ->
            { model | kickReason = value }
        , Cmd.none
        , Api.None
        )
    ResizeDesc result ->
      case result of
        Ok viewport ->
          ({ model | descHeight = viewport.scene.height }, Cmd.none, Api.None)
        Err _ ->
          (model, Cmd.none, Api.None)
    InfoLoaded game result ->
      case result of
        Ok { name, description, password, players, state } ->
          ( { model
            | game = Just game
            , name = Input.inputState name
            , desc = Input.inputState description
            , descHeight = 0
            , password = Input.inputState password
            , players = players
            , state = state
            , shuffled = False
            }
          , Cmd.batch [ NProgress.done (), resizeDesc ]
          , Api.ChangePage Pages.GameSettings
          )
        Err error ->
          (model, NProgress.done (), Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ])
    Save ->
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
          Api.setGameSettings global Saved gameID gameInfo
        Nothing ->
          Api.createGame global Saved gameInfo
      , Api.None
      )
    Saved result ->
      case result of
        Ok gameID ->
          ( { model
            | loading = False
            , game = Just gameID
            , name = Input.inputState model.name.value
            , desc = Input.inputState model.desc.value
            , password = Input.inputState model.password.value
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
      ( { model | modal = Just username, kickReason = "", kickProblem = Nothing }
      , Task.attempt (\_ -> DoNothing) (Dom.focus "kick-modal-input")
      , Api.None
      )
    HideModal ->
      ({ model | modal = Nothing }, Cmd.none, Api.None)
    Kick ->
      case (model.game, model.modal) of
        (Just gameID, Just username) ->
          ( { model | kicking = True, kickProblem = Nothing }
          , Api.kick global (Kicked username) gameID username model.kickReason
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
    Start ->
      case model.game of
        Just gameID ->
          ( { model | starting = True, problem = Nothing }
          , Api.start global Started gameID
          , Api.None
          )
        _ ->
          (model, Cmd.none, Api.None)
    Started result ->
      case result of
        Ok _ ->
          ({ model | starting = False, state = Api.Started }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | starting = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    Shuffle ->
      case model.game of
        Just gameID ->
          ( { model | shuffling = True, shuffled = False, problem = Nothing }
          , Api.shuffle global Shuffled gameID
          , Api.None
          )
        _ ->
          (model, Cmd.none, Api.None)
    Shuffled result ->
      case result of
        Ok _ ->
          ({ model | shuffling = False, shuffled = True }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | shuffling = False, shuffled = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    DoNothing ->
      (model, Cmd.none, Api.None)

discardChanges : Model -> Model
discardChanges model =
  { model
  | name = Input.initInputState
  , desc = Input.updateValue Input.initInputState "" True
  , descHeight = 0
  , password = Input.updateValue Input.initInputState "" True
  }

renderPlayer : Model -> Time.Zone -> Api.GameSettingsPlayer -> Html Msg
renderPlayer model zone player =
  div [ A.class "member-item" ]
    ([ div [ A.class "member" ]
      [ a [ A.class "link member-link", A.href ("?@" ++ player.username) ]
        [ text player.name ]
      , span [ A.class "member-info" ]
        [ text ("Joined " ++ HumanTime.display zone player.joined ++ " "
          ++ char Middot ++ " " ++ String.fromInt player.kills
          ++ (if player.kills == 1 then " elimination" else " eliminations"))
        ]
      ]
    ]
    ++ if model.state == Api.Ended then
      []
    else
      [ button [ A.class "button kick-btn", onClick (ShowModal player.username) ]
        [ text "Kick" ] ]
    ++ case model.modal of
      Just username ->
        if username == player.username then
          [ div [ A.class "modal-back show", onClick HideModal ]
            [ form
              [ A.class "modal kick-modal"
              , stopPropagationOn "click" (D.succeed (DoNothing, True))
              , onSubmit Kick
              ]
              ([ Input.myInput (Change ReasonInput)
                { myInputDefaults
                | labelText = "Kick reason (optional)"
                , placeholder = "Undesirable."
                , value = model.kickReason
                , id = Just "kick-modal-input"
                }
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

hasUnsavedChanges : Model -> Bool
hasUnsavedChanges model =
  model.name.value /= model.name.original ||
    model.desc.value /= model.desc.original ||
    model.password.value /= model.password.original

view : Api.GlobalModel m -> Model -> List (Html Msg)
view { zone } model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      (Utils.filter
        [ Just (text "Game settings")
        , Just (span [ A.class "flex" ] [])
        , if model.game /= Nothing && model.state == Api.WillStart then
          Just (button
            [ A.class "button"
            , A.classList [ ("loading", model.starting) ]
            , A.disabled (List.length model.players < 2 || model.starting)
            , onClick Start
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
        [ Input.myInput (Change NameInput)
          { myInputDefaults
          | labelText = "Name"
          , sublabel = [ text "Required." ]
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
          }
        , Input.myInput (Change PasswordInput)
          { myInputDefaults
          | labelText = "Passphrase to join"
          , sublabel = [ text "Share this passphrase to people who you want to join. Passphrases are case insensitive." ]
          , placeholder = "hunter2"
          , value = model.password.value
          , validate = \value ->
            if String.length value > 200 then
              Just "The passphrase cannot be over 200 characters long."
            else
              Nothing
          , maxChars = Just 200
          }
        ]
      , div [ A.class "input-row" ]
        [ Input.myInput (Change DescInput)
          { myInputDefaults
          | labelText = "Description and rules"
          , sublabel =
            [ text "List rules for elimination here, such as how they can be blocked, and when and where eliminations are allowed to be made. "
            , a [ A.class "link", A.href "?about#formatting" ]
              [ text "Basic formatting" ]
            , text " is available."
            ]
          , type_ = "textarea"
          , placeholder = "Please pick up the bowling balls from the front office by February 30th. Eliminations may only occur when the student is not carrying their bowling ball. Eliminations may not occur during Mr. Leasio's classes. Inappropriate behaviour will result in immediate disqualification. Targets will be shuffled every week. Good luck!"
          , value = model.desc.value
          , validate = \value ->
            if String.length value > 2000 then
              Just "Description can only be at most 2000 characters long."
            else
              Nothing
          , maxChars = Just 2000
          , id = Just "game-desc"
          , height = Just (String.fromFloat (model.descHeight + 6) ++ "px")
          }
        ]
      , let
          valid = model.name.valid &&
            model.desc.valid &&
            model.password.valid
          changed = hasUnsavedChanges model
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
        ++ (if model.state == Api.Started then
          [ button
            [ A.class "button"
            , A.classList [ ("loading", model.shuffling) ]
            , A.disabled model.shuffling
            , onClick Shuffle
            ]
            [ text (if model.shuffled then "Targets shuffled" else "Shuffle targets") ] ]
        else
          [])))
      :: (model.players
        |> List.sortBy .joined
        |> List.reverse
        |> List.map (renderPlayer model zone))
      )
    ]
  ]
