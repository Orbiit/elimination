module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)

import Base exposing (makeHeader, makeFooter)
import Pages.FrontPage exposing (frontPage)
import Pages.Terms exposing (termsPage)
import Pages.Privacy exposing (privacyPage)

type Page
  = FrontPage
  | Terms
  | Privacy

urlToPage : Url -> Page
urlToPage url =
  case url.query of
    Just path ->
      if path == "terms" then
        Terms
      else if path == "privacy" then
        Privacy
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

content : Page -> List (Html Msg)
content page =
  case page of
    FrontPage ->
      frontPage ()
    Terms ->
      termsPage ()
    Privacy ->
      privacyPage ()

view : Model -> Document Msg
view model =
  { title =
    if model.page == FrontPage then
      "Elimination"
    else
      title model.page ++ " | Elimination"
  , body = makeHeader () ++ content model.page ++ makeFooter ()
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
