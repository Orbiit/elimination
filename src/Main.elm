port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Url
import Json.Decode as D

import Base
import Api
import Pages.FrontPage
import Pages.Terms
import Pages.Privacy
import Pages.User
import Pages.Game
import Pages.UserSettings
import Pages.GameSettings

type Page
  = FrontPage
  | Terms
  | Privacy
  | User
  | Game
  | UserSettings
  | GameSettings

urlToPage : Url.Url -> Page
urlToPage url =
  case url.query of
    Just path ->
      if path == "terms" then
        Terms
      else if path == "privacy" then
        Privacy
      else if path == "user" then
        User
      else if path == "game" then
        Game
      else if path == "settings" then
        UserSettings
      else if path == "game-settings" then
        GameSettings
      else
        FrontPage
    Nothing ->
      FrontPage

type alias Model =
  { page : Page
  , key : Nav.Key
  , session : Api.Session
  , header : Base.Model
  }

init : (Maybe String, Maybe String) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init (sessionMaybe, usernameMaybe) url navKey =
  let
    session =
      case (sessionMaybe, usernameMaybe) of
        (Just sessionID, Just username) ->
          Api.SignedIn { session = sessionID, username = username }
        (_, _) ->
          Api.SignedOut
  in
    ( { page = urlToPage url
      , key = navKey
      , session = session
      , header = Base.init session
      }
    , Cmd.none
    )

title : Page -> String
title page =
  case page of
    FrontPage ->
      ""
    Terms ->
      "Terms of use"
    Privacy ->
      "Privacy policy"
    User ->
      "User"
    Game ->
      "Game"
    UserSettings ->
      "Settings"
    GameSettings ->
      "Game settings"

content : Page -> List (Html.Html Msg)
content page =
  case page of
    FrontPage ->
      Pages.FrontPage.view
    Terms ->
      Pages.Terms.view
    Privacy ->
      Pages.Privacy.view
    User ->
      Pages.User.view
    Game ->
      Pages.Game.view
    UserSettings ->
      Pages.UserSettings.view
    GameSettings ->
      Pages.GameSettings.view

view : Model -> Browser.Document Msg
view model =
  { title =
    if model.page == FrontPage then
      "Elimination"
    else
      title model.page ++ " | Elimination"
  , body
    = List.map (Html.map BaseMsg) (Base.makeHeader model.session model.header)
    ++ content model.page
    ++ Base.makeFooter
  }

type Msg
  = ChangedUrl Url.Url
  | ClickedLink Browser.UrlRequest
  | Auth String String
  | BaseMsg Base.Msg
  | Dance

port saveSession : ( String, String ) -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedLink request ->
      case request of
        Browser.Internal url ->
          ( { model | page = urlToPage url }, Nav.pushUrl model.key (Url.toString url) )
        Browser.External url ->
          ( model, Nav.load url )
    ChangedUrl url ->
      ( { model | page = urlToPage url }, Cmd.none )
    Auth session username ->
      ( { model | session = Api.SignedIn { session = session, username = username } }
      , saveSession (session, username)
      )
    BaseMsg subMsg ->
      let
        ( subModel, cmd ) = Base.update subMsg model.header
      in
        ( { model | header = subModel }, cmd )
    _ ->
      -- Disregard messages that arrived for the wrong page.
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program (Maybe String, Maybe String) Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    }
