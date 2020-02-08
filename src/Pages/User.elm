module Pages.User exposing (..)

import Html exposing (..)
import Html.Attributes as A

import Api
import NProgress
import Utils exposing (char, Char(..))
import Pages

type alias Model =
  { username : String
  , info : Api.User
  }

init : Api.Session -> Model
init _ =
  { username = ""
  , info =
    { name = ""
    , bio = ""
    , myGames = []
    , games = []
    }
  }

type Msg
  = InfoLoaded String (Result Utils.HttpError Api.User)

update : Msg -> Api.Session -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg session model =
  case msg of
    InfoLoaded username result ->
      case result of
        Ok user ->
          ({ model | username = username, info = user }, NProgress.done (), Api.ChangePage Pages.User)
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))

renderMyGame : Api.UserMyGame -> Html Msg
renderMyGame game =
  a [ A.class "item", A.href ("?!" ++ game.game) ]
    [ span [ A.class "item-name" ]
      [ text game.name ]
    , span [ A.class "item-info" ]
      [ text
        (String.fromInt game.players
        ++ (if game.players == 1 then
          " participant"
        else
          " participants")
        ++ " " ++ char Middot ++ " "
        ++ Api.gameStatusName game
        )
      ]
    ]

renderGame : Api.UserGame -> Html Msg
renderGame game =
  a [ A.class "item", A.href ("?!" ++ game.game) ]
    [ span [ A.class "item-name" ]
      [ text game.name ]
    , span [ A.class "item-info" ]
      [ text
        (String.fromInt game.players
        ++ (if game.players == 1 then
          " participant"
        else
          " participants")
        ++ " " ++ char Middot ++ " "
        ++ Api.gameStatusName game
        ++ " " ++ char Middot ++ " "
        ++ (if game.alive then "Alive" else "Eliminated")
        ++ " " ++ char Middot ++ " "
        ++ String.fromInt game.kills
        ++ (if game.kills == 1 then
          " elimination"
        else
          " eliminations"
        ))
      ]
    ]

view : Api.Session -> Model -> List (Html Msg)
view session model =
  [ article [ A.class "main content profile" ]
    [ div [ A.class "profile-info" ]
      [ h1 [ A.class "profile-name" ]
        ([ a [ A.class "link", A.href ("@" ++ model.username) ]
          [ text model.info.name ]
        , span [ A.class "flex" ]
          []
        ]
        ++ case session of
          Api.SignedIn authSession ->
            if authSession.username == model.username then
              [ a [ A.class "icon-btn settings-btn", A.href "?settings" ]
                [ text "Settings" ] ]
            else
              []
          Api.SignedOut ->
            [])
      , span [ A.class "profile-username" ]
        [ text model.username ]
      , p [ A.class "profile-desc" ]
        [ text model.info.bio ]
      , p [ A.class "profile-desc profile-stats" ]
        [ text "Total eliminations: 22" ]
      ]
    , div [ A.class "lists" ]
      (Utils.filter
        [ if List.isEmpty model.info.myGames then
          Nothing
        else
          Just (section [ A.class "list" ]
            ((h2 [] [ text "Games I created" ]) :: List.map renderMyGame model.info.myGames))
        , if List.isEmpty model.info.games then
          Nothing
        else
          Just (section [ A.class "list" ]
            ((h2 [] [ text "Games in which I participate" ]) :: List.map renderGame model.info.games))
        ])
    ]
  ]
