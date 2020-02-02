module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)

import Base exposing (makeHeader, makeFooter)
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

urlToPage : Url -> Page
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
  }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navKey =
  ( { page = urlToPage url, key = navKey }, Cmd.none )

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

content : Page -> List (Html Msg)
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

view : Model -> Document Msg
view model =
  { title =
    if model.page == FrontPage then
      "Elimination"
    else
      title model.page ++ " | Elimination"
  , body = makeHeader ++ content model.page ++ makeFooter
  }

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | Dance

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case ( msg, model ) of
    ( ClickedLink request, _ ) ->
      case request of
        Internal url ->
          ( { model | page = urlToPage url }, Nav.pushUrl model.key (Url.toString url) )
        External url ->
          ( model, Nav.load url )
    ( ChangedUrl url, _ ) ->
      ( { model | page = urlToPage url }, Cmd.none )
    ( _, _ ) ->
      -- Disregard messages that arrived for the wrong page.
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    }
