module Pages.FrontPage exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Api
import Utils exposing (char, Char(..))
import NProgress
import Pages

type alias Model =
  { stats : Api.Stats
  , statuses : List Api.Status
  }

init : Model
init =
  { stats =
    { kills = 0
    , active = 0
    , games = 0
    }
  , statuses = []
  }

type Msg
  = StatsLoaded (Result Utils.HttpError Api.Stats)
  | StatusesLoaded (Result Utils.HttpError (List Api.Status))

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg { session } model =
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
          ({ model | statuses = statuses }
          , NProgress.done ()
          , Api.ChangePage Pages.FrontPage
          )
        Err ((_, errorMsg) as error) ->
          ( model
          , NProgress.done ()
          , Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ]
          )

renderStatus : Api.Status -> Html msg
renderStatus status =
  article [ A.class "target", A.style "background-color" "rgba(255, 0, 0, 0.12)"  ]
    [ a [ A.class "game-link link", A.href ("?!" ++ status.game) ]
      [ text status.gameName ]
    , span [ A.class "flex" ]
      []
    , span [ A.class "target-label" ]
      [ text "Your target is" ]
    , a [ A.class "target-name link", A.href ("?@" ++ status.target) ]
      [ text status.targetName ]
    , button [ A.class "button kill-btn" ]
      [ text "Eliminate" ]
    , div [ A.class "modal-back" ]
      [ form [ A.class "modal join-modal" ]
        [ label [ A.class "input-wrapper" ]
          [ span [ A.class "label" ]
            [ text "Target's elimination sequence" ]
          , div [ A.class "input" ]
            [ input [ A.name "password", A.placeholder "hunter2", A.required True, A.type_ "text" ]
              []
            ]
          ]
        , input
          [ A.class "button submit-btn"
          , A.type_ "submit"
          , A.value "Eliminate"
          ]
          []
        ]
      ]
    , span [ A.class "flex" ]
      []
    , span [ A.class "kill-code" ]
      [ text "Click to reveal your elimination sequence: "
      , span [ A.class "code" ]
        [ text status.code ]
      , text (" " ++ char MDash ++ " ")
      , a [ A.class "link", A.href "?about#kill-codes" ]
        [ text "What is this for?" ]
      ]
    , span [ A.class "flex" ]
      []
    ]

view : Api.GlobalModel m -> Model -> List (Html msg)
view { session } model =
  case session of
    Api.SignedIn _ ->
      [ div [ A.class "main targets" ]
        ((a [ A.class "button small-screen-create-game-btn", A.href "?create-game" ]
          [ text "Create game" ])
        :: if List.isEmpty model.statuses then
          [ p [ A.class "no-statuses" ] [ text "You aren't in any active games." ] ]
        else
          List.map renderStatus model.statuses)
      ]
    Api.SignedOut ->
      [ div [ A.class "main front-text" ]
        [ h1 [ A.class "website-title" ]
          [ text "Elimination" ]
        ]
      , article [ A.class "main content welcome" ]
        [ p []
          [ text "Elimination is a game where" ]
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
