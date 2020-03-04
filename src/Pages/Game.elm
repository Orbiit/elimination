module Pages.Game exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, stopPropagationOn, onSubmit)
import Json.Decode as D
import Browser.Dom as Dom
import Task
import Dict exposing (Dict)

import Api
import Utils exposing (char, Char(..))
import Utils.Input as Input exposing (myInputDefaults)
import Utils.HumanTime as HumanTime
import Utils.MarkupSimpleRegex as Markup
import Pages
import NProgress

type alias ElimTimes = Dict String Api.Timestamp

type alias Model =
  { game : Api.GameID
  , info : Api.Game
  , elimTimes : ElimTimes
  , description : List (Html Msg)
  , password : String
  , modal : Bool
  , loading : Bool
  , problem : Maybe String
  }

init : Model
init =
  { game = ""
  , info =
    { creator = ("", "")
    , name = ""
    , description = ""
    , joinDisabled = False
    , players = []
    , state = Api.WillStart
    , time = 0
    , announcements = []
    }
  , elimTimes = Dict.empty
  , description = []
  , password = ""
  , modal = False
  , loading = False
  , problem = Nothing
  }

type Msg
  = InfoLoaded Api.GameID (Api.Response Api.Game)
  | NamesLoaded (Api.Response (Dict Api.GameID String))
  | ChangePassword Input.MyInputMsg
  | ShowModal
  | HideModal
  | Join
  | Leave
  | Done BtnAction (Api.Response String)
  | DoNothing

type BtnAction
  = Joining String
  | Leaving String

-- Get the last time a player killed someone
getElimTime : Api.GamePlayer -> ElimTimes -> ElimTimes
getElimTime player times =
  case (player.killer, player.killTime) of
    -- If player was killed
    (Just killer, Just killTime) ->
      let
        time =
          -- Find their killer
          case Dict.get killer times of
            Just otherKillTime ->
              -- Take most recent kill
              max otherKillTime killTime
            Nothing ->
              killTime
      in
      Dict.insert killer time times
    _ ->
      times

getElimTimes : List Api.GamePlayer -> ElimTimes
getElimTimes players =
  List.foldl getElimTime Dict.empty players

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    InfoLoaded gameID result ->
      case result of
        Ok game ->
          let
            description = Markup.markup Nothing game.description
          in
          ( { model
            | game = gameID
            , info = game
            , elimTimes = getElimTimes game.players
            , description = description.html
            }
          , Cmd.batch
            [ NProgress.done ()
            , if List.isEmpty description.gameIDs then
                Cmd.none
              else
                Api.getNames global NamesLoaded description.gameIDs
            ]
          , Api.ChangePage Pages.Game
          )
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))
    NamesLoaded result ->
      case result of
        Ok names ->
          ({ model | description = (Markup.markup (Just names) model.info.description).html }, Cmd.none, Api.None)
        Err _ ->
          (model, Cmd.none, Api.None)
    ChangePassword { value } ->
      ({ model | password = value }, Cmd.none, Api.None)
    ShowModal ->
      ( { model | modal = True }
      , Task.attempt (\_ -> DoNothing) (Dom.focus "join-modal-input")
      , Api.None
      )
    HideModal ->
      ({ model | modal = False }, Cmd.none, Api.None)
    Join ->
      case global.session of
        Api.SignedIn { username } ->
          ( { model | loading = True, problem = Nothing }
          , Api.join global (Done (Joining username)) model.game model.password
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Leave ->
      case global.session of
        Api.SignedIn { username } ->
          ( { model | loading = True, problem = Nothing }
          , Api.leave global (Done (Leaving username)) model.game
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Done action result ->
      let
        info = model.info
      in
      case (result, action) of
        (Ok name, Joining username) ->
          ( { model
            | info =
              { info
              | players =
                { username = username
                , name = name
                , alive = True
                , killTime = Nothing
                , killer = Nothing
                , killerName = Nothing
                , kills = 0
                -- Max safe integer; this is so the user is on top
                , joined = 2 ^ 53 - 1
                }
                :: info.players
              }
            , password = ""
            , modal = False
            , loading = False
            }
          , Cmd.none
          , Api.None
          )
        (Err ((_, errorMsg) as error), Joining _) ->
          ({ model | loading = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
        (_, Leaving username) ->
          ( { model
            | info =
              { info
              | players =
                List.filter (\player -> player.username /= username) info.players
              }
            , loading = False
            }
          , Cmd.none
          , Api.None
          )
    DoNothing ->
      (model, Cmd.none, Api.None)

renderPlayer : Api.GlobalModel m -> Bool -> Api.GamePlayer -> Html Msg
renderPlayer global ended player =
  a
    [ A.class "item"
    , A.classList [ ("dead", not player.alive), ("winner", ended && player.alive) ]
    , A.href ("?@" ++ player.username)
    ]
    [ span [ A.class "item-name" ]
      [ text player.name ]
    , span [ A.class "item-info" ]
      [ text <| String.concat
        [ case (player.killTime, player.killer, player.killerName) of
          (Just time, Just killer, Just killerName) ->
            "Eliminated on " ++ HumanTime.display global.zone time ++
              " by " ++ killerName
          _ ->
            if ended then "Winner" else "Alive"
        , " " ++ char Middot ++ " "
        , String.fromInt player.kills
        , if player.kills == 1 then
          " elimination"
        else
          " eliminations"
        ]
      ]
    ]

renderAnnouncement : Api.GlobalModel m -> Api.GameAnnouncement -> Html Msg
renderAnnouncement global announcement =
  div [ A.class "item" ]
    [ div [ A.class "announcement" ]
      [ text announcement.message ]
    , span [ A.class "item-info" ]
      [ text <| String.concat
        [ HumanTime.display global.zone announcement.time
        , " " ++ char Middot ++ " "
        , if announcement.includedDead then
          "To all players"
        else
          "To alive players only"
        ]
      ]
    ]

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  [ article [ A.class "main content profile" ]
    [ div [ A.class "profile-info" ]
      [ h1 [ A.class "profile-name" ]
        ([ a [ A.class "link", A.href ("?!" ++ model.game) ]
          [ text model.info.name ]
        , span [ A.class "flex" ] [ text " " ]
        ]
        ++ case global.session of
          Api.SignedIn { username } ->
            [ if username == Tuple.first model.info.creator then
              a [ A.class "icon-btn settings-btn", A.href ("?settings!" ++ model.game) ]
                [ text "Settings" ]
            else
              text ""
            , if model.info.state /= Api.WillStart then
              text ""
            else if List.any (\player -> player.username == username) model.info.players then
              button
                [ A.class "button join-btn"
                , A.classList [ ("loading", model.loading) ]
                , A.disabled model.loading
                , onClick Leave
                ]
                [ text "Leave" ]
            else if model.info.joinDisabled then
              button [ A.class "button join-btn", A.disabled True ]
                [ text "Joining has been disabled" ]
            else
              button [ A.class "button join-btn", onClick ShowModal ]
                [ text "Join" ]
            ]
          Api.SignedOut ->
            if model.info.state /= Api.WillStart then
              []
            else if model.info.joinDisabled then
              [ button [ A.class "button join-btn", A.disabled True ]
                [ text "Joining has been disabled" ]
              ]
            else
              [ button [ A.class "button join-btn", A.disabled True ]
                [ text "You need to be signed in to join" ]
              ]
        )
      , div [ A.class "modal-back", A.classList [ ("show", model.modal) ], onClick HideModal ]
        [ form
          [ A.class "modal join-modal"
          , stopPropagationOn "click" (D.succeed (DoNothing, True))
          , onSubmit Join
          ]
          [ Input.myInput ChangePassword
            { myInputDefaults
            | labelText = "Passphrase"
            , placeholder = "hunter2"
            , value = model.password
            , id = Just "join-modal-input"
            , attributes =
              [ A.attribute "autocapitalize" "none"
              , A.attribute "autocorrect" "off"
              ]
            }
          , input
            [ A.class "button submit-btn"
            , A.classList [ ("loading", model.loading) ]
            , A.type_ "submit"
            , A.value "Join"
            , A.disabled model.loading
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
      , span [ A.class "profile-subtitle" ]
        [ text "Created by "
        , let
            (creator, creatorName) = model.info.creator
          in
          a [ A.class "link", A.href ("?@" ++ creator) ]
            [ text creatorName ]
        ]
      , p [ A.class "profile-desc" ]
        model.description
      , p [ A.class "profile-desc profile-stats" ]
        [ text (Api.gameStateNameWithTime global.zone model.info.state model.info.time
          ++ " " ++ char Middot ++ " "
          ++ String.fromInt (List.length (List.filter (\player -> player.alive) model.info.players))
          ++ " of " ++ String.fromInt (List.length model.info.players)
          ++ (if List.length model.info.players == 1 then " participant" else " participants")
          ++ " alive"
        ) ]
      ]
    , div [ A.class "lists" ]
      [ if List.isEmpty model.info.announcements then
        text ""
      else
        section [ A.class "list" ] <|
          (h2 [] [ text "Announcements" ])
          :: (model.info.announcements
            |> List.sortBy .time
            |> List.reverse
            |> List.map (renderAnnouncement global))
      , section [ A.class "list" ] <|
        (h2 []
          [ text
            (if model.info.state == Api.WillStart then
              "Participants (" ++ (String.fromInt (List.length model.info.players)) ++ ")"
            else
              "Leaderboard by most eliminations")
          ])
        :: (model.info.players
          |> List.sortWith (\a -> \b ->
            case compare b.kills a.kills of
              EQ ->
                case (a.killTime, b.killTime) of
                  (Just aKillTime, Just bKillTime) ->
                    compare bKillTime aKillTime
                  (Nothing, Just _) ->
                    LT
                  (Just _, Nothing) ->
                    GT
                  (Nothing, Nothing) ->
                    case (Dict.get a.username model.elimTimes, Dict.get b.username model.elimTimes) of
                      (Just aKillTime, Just bKillTime) ->
                        compare bKillTime aKillTime
                      _ ->
                        compare b.joined a.joined
              _ as order ->
                order
          )
          |> List.map (renderPlayer global (model.info.state == Api.Ended)))
      ]
    ]
  ]
