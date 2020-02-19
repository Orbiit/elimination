module Pages.Game exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, stopPropagationOn, onSubmit)
import Json.Decode as D
import Browser.Dom as Dom
import Task

import Api
import Utils exposing (char, Char(..))
import Utils.Input as Input exposing (myInputDefaults)
import Utils.HumanTime as HumanTime
import Utils.Request as Request
import Utils.MarkupSimpleRegex as Markup
import Pages
import NProgress

type alias Model =
  { game : Api.GameID
  , info : Api.Game
  , password : String
  , modal : Bool
  , loading : Bool
  , problem : Maybe String
  }

init : Model
init =
  { game = ""
  , info =
    { creator = ""
    , creatorName = ""
    , name = ""
    , description = ""
    , players = []
    , state = Api.WillStart
    , time = 0
    }
  , password = ""
  , modal = False
  , loading = False
  , problem = Nothing
  }

type Msg
  = InfoLoaded Api.GameID (Result Request.HttpError Api.Game)
  | ChangePassword Input.MyInputMsg
  | ShowModal
  | HideModal
  | Join
  | Leave
  | Done BtnAction (Result Request.HttpError String)
  | DoNothing

type BtnAction
  = Joining String
  | Leaving String

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    InfoLoaded gameID result ->
      case result of
        Ok game ->
          ({ model | game = gameID, info = game }, NProgress.done (), Api.ChangePage Pages.Game)
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))
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
      case result of
        Ok name ->
          let
            info = model.info
            newModel =
              case action of
                Joining username ->
                  { model | info = { info | players =
                    { username = username
                    , name = name
                    , alive = True
                    , killTime = Nothing
                    , killer = Nothing
                    , killerName = Nothing
                    , kills = 0
                    }
                    :: info.players
                  }, password = "" }
                Leaving username ->
                  { model | info = { info | players =
                    List.filter (\player -> player.username /= username) info.players
                  } }
          in
            ({ newModel | modal = False, loading = False }, Cmd.none, Api.None)
        Err ((_, errorMsg) as error) ->
          ({ model | loading = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error)
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
      [ text
        ((case (player.killTime, player.killer, player.killerName) of
          (Just time, Just killer, Just killerName) ->
            "Eliminated on " ++ HumanTime.display global.zone time ++
              " by " ++ killerName
          _ ->
            if ended then "Winner" else "Alive")
        ++ " " ++ char Middot ++ " "
        ++ String.fromInt player.kills
        ++ (if player.kills == 1 then
          " elimination"
        else
          " eliminations"))
      ]
    ]

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  [ article [ A.class "main content profile" ]
    [ div [ A.class "profile-info" ]
      [ h1 [ A.class "profile-name" ]
        ([ a [ A.class "link", A.href ("?!" ++ model.game) ]
          [ text model.info.name ]
        , span [ A.class "flex" ]
          []
        ]
        ++ case global.session of
          Api.SignedIn { username } ->
            Utils.filter
              [ if username == model.info.creator then
                Just (a [ A.class "icon-btn settings-btn", A.href ("?settings!" ++ model.game) ]
                  [ text "Settings" ])
              else
                Nothing
              , if model.info.state /= Api.WillStart then
                Nothing
              else if List.any (\player -> player.username == username) model.info.players then
                Just (button
                  [ A.class "button join-btn"
                  , A.classList [ ("loading", model.loading) ]
                  , A.disabled model.loading
                  , onClick Leave
                  ]
                  [ text "Leave" ])
              else
                Just (button [ A.class "button join-btn", onClick ShowModal ]
                  [ text "Join" ])
              ]
          Api.SignedOut ->
            if model.info.state == Api.WillStart then
              [ button [ A.class "button join-btn", A.disabled True ]
                [ text "You need to be signed in to join" ]
              ]
            else
              []
        )
      , div [ A.class "modal-back", A.classList [ ("show", model.modal) ], onClick HideModal ]
        [ form
          [ A.class "modal join-modal"
          , stopPropagationOn "click" (D.succeed (DoNothing, True))
          , onSubmit Join
          ]
          ([ Input.myInput ChangePassword
            { myInputDefaults
            | labelText = "Passphrase"
            , placeholder = "hunter2"
            , value = model.password
            , id = Just "join-modal-input"
            }
          , input
            [ A.class "button submit-btn"
            , A.classList [ ("loading", model.loading) ]
            , A.type_ "submit"
            , A.value "Join"
            , A.disabled model.loading
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
      , span [ A.class "profile-subtitle" ]
        [ text "Created by "
        , a [ A.class "link", A.href ("?@" ++ model.info.creator) ]
          [ text model.info.creatorName ]
        ]
      , p [ A.class "profile-desc" ]
        (Markup.markup model.info.description)
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
      [ section [ A.class "list" ]
        ((h2 []
          [ text ("Participants (" ++ (String.fromInt (List.length model.info.players)) ++ ")") ])
        :: (List.map (renderPlayer global (model.info.state == Api.Ended))
          (List.sortWith (\a -> \b ->
            case compare b.kills a.kills of
              EQ ->
                if a.alive && not b.alive then
                  LT
                else if b.alive && not a.alive then
                  GT
                else
                  EQ
              _ as order ->
                order
          ) model.info.players))
        )
      ]
    ]
  ]
