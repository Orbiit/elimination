module Pages.User exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Dict exposing (Dict)

import Api
import NProgress
import Utils exposing (char, Char(..))
import Utils.MarkupSimpleRegex as Markup
import Pages

type alias Model =
  { username : String
  , info : Api.User
  , bio : List (Html Msg)
  }

init : Model
init =
  { username = ""
  , info =
    { name = ""
    , bio = ""
    , myGames = []
    , games = []
    }
  , bio = []
  }

type Msg
  = InfoLoaded String (Api.Response Api.User)
  | NamesLoaded (Api.Response (Dict Api.GameID String))

update : Msg -> Api.GlobalModel m -> Model -> (Model, Cmd Msg, Api.PageCmd)
update msg global model =
  case msg of
    InfoLoaded username result ->
      case result of
        Ok user ->
          let
            bio = Markup.markup Nothing user.bio
          in
          ( { model | username = username, info = user, bio = bio.html }
          , Cmd.batch
            [ NProgress.done ()
            , if List.isEmpty bio.gameIDs then
                Cmd.none
              else
                Api.getNames global NamesLoaded bio.gameIDs
            ]
          , Api.ChangePage Pages.User
          )
        Err error ->
          (model, NProgress.done (), Api.ChangePage (Pages.Error error))
    NamesLoaded result ->
      case result of
        Ok names ->
          ({ model | bio = (Markup.markup (Just names) model.info.bio).html }, Cmd.none, Api.None)
        Err _ ->
          (model, Cmd.none, Api.None)

renderMyGame : Api.GlobalModel m -> Api.UserMyGame -> Html Msg
renderMyGame global game =
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
        ++ Api.gameStateNameWithTime global.zone game.state game.time
        )
      ]
    ]

renderGame : Api.UserGame -> Html Msg
renderGame game =
  a
    [ A.class "item"
    , A.classList [ ("winner", game.state == Api.Ended && game.alive) ]
    , A.href ("?!" ++ game.game)
    ]
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
        ++ Api.gameStateName game.state
        ++ " " ++ char Middot ++ " "
        ++ (if game.alive then (if game.state == Api.Ended then "Winner" else "Alive") else "Eliminated")
        ++ " " ++ char Middot ++ " "
        ++ String.fromInt game.kills
        ++ (if game.kills == 1 then
          " elimination"
        else
          " eliminations"
        ))
      ]
    ]

view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
  [ article [ A.class "main content profile" ]
    [ div [ A.class "profile-info" ]
      [ h1 [ A.class "profile-name" ]
        ([ a [ A.class "link", A.href ("?@" ++ model.username) ]
          [ text model.info.name ]
        , span [ A.class "flex" ] [ text " " ]
        ]
        ++ case global.session of
          Api.SignedIn { username } ->
            if username == model.username then
              [ a [ A.class "icon-btn settings-btn", A.href "?settings" ]
                [ text "Settings" ] ]
            else
              []
          Api.SignedOut ->
            [])
      , span [ A.class "profile-subtitle" ]
        [ text ("@" ++ model.username) ]
      , p [ A.class "profile-desc" ]
        model.bio
      , p [ A.class "profile-desc profile-stats" ]
        [ text ("Total eliminations: " ++ String.fromInt (List.sum (List.map .kills model.info.games))) ]
      ]
    , div [ A.class "lists" ]
      (Utils.filter
        [ if List.isEmpty model.info.myGames then
          Nothing
        else
          Just (section [ A.class "list" ]
            ((h2 [] [ text ("Games I created (" ++ String.fromInt (List.length model.info.myGames) ++ ")") ]) ::
              (model.info.myGames
                |> List.sortBy .time
                |> List.reverse
                |> List.map (renderMyGame global))))
        , if List.isEmpty model.info.games then
          Nothing
        else
          Just (section [ A.class "list" ]
            ((h2 [] [ text ("Games in which I participate (" ++ String.fromInt (List.length model.info.games) ++ ")") ]) ::
              (model.info.games
                |> List.sortBy .updated
                |> List.reverse
                |> List.map renderGame)))
        ])
    ]
  ]
