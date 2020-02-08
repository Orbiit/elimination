module Pages.Game exposing (..)

import Html exposing (..)
import Html.Attributes as A

import Api
import Utils exposing (char, Char(..))
import Pages
import NProgress

type alias Model =
  { game : Api.GameID
  , info : Api.Game
  }

init : Api.Session -> Model
init _ =
  { game = ""
  , info =
    { owner = ""
    , ownerName = ""
    , name = ""
    , description = ""
    , players = []
    , started = False
    , ended = False
    }
  }

type Msg
  = InfoLoaded Api.GameID (Result Utils.HttpError Api.Game)

update : Msg -> Api.Session -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg session model =
  case msg of
    InfoLoaded gameID result ->
      case result of
        Ok game ->
          ({ model | game = gameID, info = game }, NProgress.done (), Api.ChangePage Pages.Game)
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))

view : Api.Session -> Model -> List (Html msg)
view session model =
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
              [ if username == model.info.owner then
                Just (a [ A.class "icon-btn settings-btn", A.href ("?settings!" ++ model.game) ]
                  [ text "Settings" ])
              else
                Nothing
              , if List.any (\player -> player.username == username) model.info.players then
                Just (button [ A.class "button join-btn" ]
                  [ text "Leave" ])
              else
                Just (button [ A.class "button join-btn" ]
                  [ text "Join" ])
              ]
          Api.SignedOut ->
            []
        )
      , div [ A.class "modal-back" ]
        [ form [ A.class "modal join-modal" ]
          [ label [ A.class "input-wrapper" ]
            [ span [ A.class "label" ]
              [ text "Passphrase" ]
            , div [ A.class "input" ]
              [ input [ A.name "password", A.placeholder "hunter2", A.attribute "required" "", A.type_ "text" ]
                []
              ]
            ]
          , input [ A.class "button submit-btn", A.type_ "submit", A.value "Join" ]
            []
          ]
        ]
      , p [ A.class "profile-desc" ]
        [ text model.info.description ]
      , p [ A.class "profile-desc profile-stats" ]
        [ text (Api.gameStatusName model.info ++ " " ++ char Middot ++ " "
          ++ String.fromInt (List.length (List.filter (\player -> player.alive) model.info.players))
          ++ " of " ++ String.fromInt (List.length model.info.players)
          ++ if List.length model.info.players == 1 then " participant" else " participants"
          ++ " alive"
        ) ]
      ]
    , div [ A.class "lists" ]
      [ section [ A.class "list" ]
        [ h2 []
          [ text "Participants (6)" ]
        , a [ A.class "item", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Jame Sooth" ]
          , span [ A.class "item-info" ]
            [ text "Alive · 2 eliminations" ]
          ]
        , a [ A.class "item dead", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Sergo Neristo" ]
          , span [ A.class "item-info" ]
            [ text "Eliminated · 1 eliminations" ]
          ]
        , a [ A.class "item", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Ern Seth" ]
          , span [ A.class "item-info" ]
            [ text "Alive · 0 eliminations" ]
          ]
        , a [ A.class "item", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Memphie Ratch" ]
          , span [ A.class "item-info" ]
            [ text "Alive · 0 eliminations" ]
          ]
        , a [ A.class "item dead", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Eghten Siuvoulet" ]
          , span [ A.class "item-info" ]
            [ text "Eliminated · 0 eliminations" ]
          ]
        , a [ A.class "item dead", A.href "./user.html" ]
          [ span [ A.class "item-name" ]
            [ text "Zuck Ergostan" ]
          , span [ A.class "item-info" ]
            [ text "Eliminated · 0 eliminations" ]
          ]
        ]
      ]
    ]
  ]
