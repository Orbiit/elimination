module Pages.FrontPage exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onSubmit, stopPropagationOn, onClick)
import Json.Decode as D

import Api
import Utils exposing (char, Char(..), myInputDefaults)
import NProgress
import Pages

type alias Model =
  { stats : Api.Stats
  , statuses : List Api.Status
  , modal : Maybe Api.GameID
  , code : String
  , killing : Bool
  , problem : Maybe String
  , showingCode : Maybe Api.GameID
  }

init : Model
init =
  { stats =
    { kills = 0
    , active = 0
    , games = 0
    }
  , statuses = []
  , modal = Nothing
  , code = ""
  , killing = False
  , problem = Nothing
  , showingCode = Nothing
  }

type Msg
  = StatsLoaded (Result Utils.HttpError Api.Stats)
  | StatusesLoaded (Result Utils.HttpError (List Api.Status))
  | ShowModal Api.GameID
  | HideModal
  | ChangeCode Utils.MyInputMsg
  | Kill
  | Killed (Result Utils.HttpError ())
  | ShowCode Api.GameID
  | DoNothing

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    StatsLoaded result ->
      case result of
        Ok stats ->
          ({ model | stats = stats }, NProgress.done (), Api.ChangePage Pages.FrontPage)
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))
    StatusesLoaded result ->
      case result of
        Ok statuses ->
          ({ model | statuses = statuses, showingCode = Nothing }
          , NProgress.done ()
          , Api.ChangePage Pages.FrontPage
          )
        Err ((_, errorMsg) as error) ->
          ( model
          , NProgress.done ()
          , Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ]
          )
    ShowModal game ->
      ({ model | modal = Just game, code = "", problem = Nothing }, Cmd.none, Api.None)
    HideModal ->
      ({ model | modal = Nothing }, Cmd.none, Api.None)
    ChangeCode { value } ->
      ({ model | code = value }, Cmd.none, Api.None)
    Kill ->
      case (global.session, model.modal) of
        (Api.SignedIn { session }, Just game) ->
          ( { model | problem = Nothing, killing = True }
          , Api.kill model.code game session Killed
          , Api.None
          )
        _ ->
          (model, Cmd.none, Api.None)
    Killed result ->
      case result of
        Ok _ ->
          ( { model | killing = False, modal = Nothing }
          , case global.session of
            Api.SignedIn { session } ->
              Cmd.batch
                [ Api.statuses session StatusesLoaded
                , NProgress.start ()
                ]
            Api.SignedOut ->
              Cmd.none
          , Api.None
          )
        Err ((_, errorMsg) as error) ->
          ({ model | killing = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
    ShowCode game ->
      ({ model | showingCode = Just game }, Cmd.none, Api.None)
    DoNothing ->
      (model, Cmd.none, Api.None)

renderStatus : Model -> Api.Status -> Html Msg
renderStatus model status =
  article
    [ A.class "target"
    , let
        -- Mess around with numbers for a "random"-ish number; `alpha` will
        -- become some float between 0 and 0.2
        number = List.sum (List.map Char.toCode (String.toList status.gameName))
        alpha = toFloat (modBy 793 number) / (793 / 0.2)
      in
        A.style "background-color" ("rgba(255, 0, 0, " ++ String.fromFloat alpha ++ ")")
    ]
    [ a [ A.class "game-link link", A.href ("?!" ++ status.game) ]
      [ text status.gameName ]
    , span [ A.class "flex" ]
      []
    , span [ A.class "target-label" ]
      [ text "Your target is" ]
    , a [ A.class "target-name link", A.href ("?@" ++ status.target) ]
      [ text status.targetName ]
    , button [ A.class "button kill-btn", onClick (ShowModal status.game) ]
      [ text "Eliminate" ]
    , case model.modal of
      Just modal ->
        if modal == status.game then
          div [ A.class "modal-back show", onClick HideModal ]
            [ form
              [ A.class "modal join-modal"
              , stopPropagationOn "click" (D.succeed (DoNothing, True))
              , onSubmit Kill
              ]
              ([ Utils.myInput ChangeCode
                { myInputDefaults
                | labelText = "Target's elimination sequence"
                , placeholder = "hunter2"
                , value = model.code
                }
              , input
                [ A.class "button submit-btn"
                , A.classList [ ("loading", model.killing) ]
                , A.type_ "submit"
                , A.value "Eliminate"
                , A.disabled model.killing
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
        else
          text ""
      _ ->
        text ""
    , span [ A.class "flex" ]
      []
    , span [ A.class "kill-code" ]
      [ text "Click to reveal your elimination sequence: "
      , let
          showing =
            case model.showingCode of
              Just game ->
                game == status.game
              Nothing ->
                False
        in
          span
            [ A.class "code"
            , A.classList [ ("revealed", showing) ]
            , onClick (if showing then DoNothing else ShowCode status.game)
            ]
            [ text status.code ]
      , text (" " ++ char MDash ++ " ")
      , a [ A.class "link", A.href "?about#kill-codes" ]
        [ text "What is this for?" ]
      ]
    , span [ A.class "flex" ]
      []
    ]

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  case global.session of
    Api.SignedIn _ ->
      [ div [ A.class "main targets" ]
        ((a [ A.class "button small-screen-create-game-btn", A.href "?create-game" ]
          [ text "Create game" ])
        :: if List.isEmpty model.statuses then
          [ p [ A.class "no-statuses" ] [ text "You aren't in any active games." ] ]
        else
          List.map (renderStatus model) model.statuses)
      ]
    Api.SignedOut ->
      [ div [ A.class "main front-text" ]
        [ h1 [ A.class "website-title" ]
          [ text "Elimination" ]
        ]
      , article [ A.class "main content welcome" ]
        [ p []
          [ text "Elimination is a work in progress right now!" ]
        , div [ A.class "stats" ]
          [ div [ A.class "stat" ]
            [ span [ A.class "stat-name" ]
              [ text "Players eliminated" ]
            , span [ A.class "stat-value" ]
              [ text (String.fromInt model.stats.kills) ]
            ]
          , div [ A.class "stat" ]
            [ span [ A.class "stat-name" ]
              [ text "Active games" ]
            , span [ A.class "stat-value" ]
              [ text (String.fromInt model.stats.active) ]
            ]
          , div [ A.class "stat" ]
            [ span [ A.class "stat-name" ]
              [ text "Total games" ]
            , span [ A.class "stat-value" ]
              [ text (String.fromInt model.stats.games) ]
            ]
          ]
        ]
      ]
