module Pages.User exposing (..)

import Html exposing (..)
import Html.Attributes as A

import Api
import NProgress
import Utils
import Pages

type alias Model =
  { username : String
  , info : Api.User
  }

init : Api.Session -> Model
init session =
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

view : Api.Session -> Model -> List (Html msg)
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
      [ section [ A.class "list" ]
        [ h2 []
          [ text "Games I created" ]
        , a [ A.class "item", A.href "./game.html" ]
          [ span [ A.class "item-name" ]
            [ text "Mr. Munkler's E period knife practice" ]
          , span [ A.class "item-info" ]
            [ text "24 participants · Ended" ]
          ]
        ]
      , section [ A.class "list" ]
        [ h2 []
          [ text "Games in which I participate" ]
        , a [ A.class "item", A.href "./game.html" ]
          [ span [ A.class "item-name" ]
            [ text "Pistole High School Elimination 2020" ]
          , span [ A.class "item-info" ]
            [ text "302 participants · 16 eliminations · Alive · Ongoing" ]
          ]
        , a [ A.class "item", A.href "./game.html" ]
          [ span [ A.class "item-name" ]
            [ text "Pistole High School eliminations 2017" ]
          , span [ A.class "item-info" ]
            [ text "278 participants · 0 eliminations · Eliminated · Ended" ]
          ]
        , a [ A.class "item", A.href "./game.html" ]
          [ span [ A.class "item-name" ]
            [ text "Pistole HS Elimination 2016" ]
          , span [ A.class "item-info" ]
            [ text "230 participants · 6 eliminations · Eliminated · Ended" ]
          ]
        ]
      ]
    ]
  ]
