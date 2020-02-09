module Pages.Game exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, stopPropagationOn, onSubmit)
import Json.Decode as D

import Api
import Utils exposing (char, Char(..))
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
    , started = False
    , ended = False
    }
  , password = ""
  , modal = False
  , loading = False
  , problem = Nothing
  }

type Msg
  = InfoLoaded Api.GameID (Result Utils.HttpError Api.Game)
  | ChangePassword (String -> Maybe String) String
  | ShowModal
  | HideModal
  | Join
  | Leave
  | Done BtnAction (Result Utils.HttpError ())
  | DontClose

type BtnAction
  = Joining String
  | Leaving String

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg { session } model =
  case msg of
    InfoLoaded gameID result ->
      case result of
        Ok game ->
          ({ model | game = gameID, info = game }, NProgress.done (), Api.ChangePage Pages.Game)
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))
    ChangePassword _ value ->
      ({ model | password = value }, Cmd.none, Api.None)
    ShowModal ->
      ({ model | modal = True }, Cmd.none, Api.None)
    HideModal ->
      ({ model | modal = False }, Cmd.none, Api.None)
    Join ->
      case session of
        Api.SignedIn ({ username } as authSession) ->
          ( { model | loading = True, problem = Nothing }
          , Api.join model.password model.game authSession.session (Done (Joining username))
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Leave ->
      case session of
        Api.SignedIn ({ username } as authSession) ->
          ( { model | loading = True, problem = Nothing }
          , Api.leave model.game authSession.session (Done (Leaving username))
          , Api.None
          )
        Api.SignedOut ->
          (model, Cmd.none, Api.None)
    Done action result ->
      case result of
        Ok _ ->
          let
            info = model.info
            newModel =
              case action of
                Joining username ->
                  { model | info = { info | players =
                    { username = username
                    , name = "You"
                    , alive = True
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
    DontClose ->
      (model, Cmd.none, Api.None)

renderPlayer : Api.GamePlayer -> Html Msg
renderPlayer player =
  a
    [ A.class "item"
    , A.classList [ ("dead", not player.alive) ]
    , A.href ("?@" ++ player.username)
    ]
    [ span [ A.class "item-name" ]
      [ text player.name ]
    , span [ A.class "item-info" ]
      [ text
        ((if player.alive then "Alive" else "Eliminated")
        ++ " " ++ char Middot ++ " "
        ++ String.fromInt player.kills
        ++ (if player.kills == 1 then
          " elimination"
        else
          " eliminations"))
      ]
    ]

view : Api.GlobalModel m -> Model -> List (Html Msg)
view { session } model =
  [ article [ A.class "main content profile" ]
    [ div [ A.class "profile-info" ]
      [ h1 [ A.class "profile-name" ]
        ([ a [ A.class "link", A.href ("?!" ++ model.game) ]
          [ text model.info.name ]
        , span [ A.class "flex" ]
          []
        ]
        ++ case session of
          Api.SignedIn { username } ->
            Utils.filter
              [ if username == model.info.creator then
                Just (a [ A.class "icon-btn settings-btn", A.href ("?settings!" ++ model.game) ]
                  [ text "Settings" ])
              else
                Nothing
              , if model.info.started then
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
            []
        )
      , div [ A.class "modal-back", A.classList [ ("show", model.modal) ], onClick HideModal ]
        [ form
          [ A.class "modal join-modal"
          , stopPropagationOn "click" (D.succeed (DontClose, True))
          , onSubmit Join
          ]
          ([ Utils.myInput
            { labelText = "Passphrase"
            , sublabel = ""
            , type_ = "text"
            , placeholder = "hunter2"
            , value = model.password
            , validate = \value -> Nothing
            , maxChars = Nothing
            , storeValueMsg = ChangePassword }
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
        [ text model.info.description ]
      , p [ A.class "profile-desc profile-stats" ]
        [ text (Api.gameStatusName model.info ++ " " ++ char Middot ++ " "
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
        :: (List.map renderPlayer
          (List.sortWith (\a -> \b ->
            case compare a.kills b.kills of
              EQ ->
                if a.alive && not b.alive then
                  GT
                else if b.alive && not a.alive then
                  LT
                else
                  EQ
              _ as order ->
                order
          ) model.info.players))
        )
      ]
    ]
  ]
