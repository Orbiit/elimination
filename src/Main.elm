port module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html
import Url
import Url.Builder
import Json.Decode as D

import Base
import Api
import Utils
import Pages.FrontPage
import Pages.Terms
import Pages.Privacy
import Pages.User
import Pages.Game
import Pages.UserSettings
import Pages.GameSettings
import Pages.Loading
import Pages.Error

type Page
  = FrontPage
  | Terms
  | Privacy
  | User
  | Game
  | UserSettings
  | GameSettings
  | Loading
  | Error Utils.HttpError

type PageCmd
  = LoadPage Page
  | Command (Cmd Msg)

urlToPage : Url.Url -> Api.Session -> PageCmd
urlToPage url session =
  case url.query of
    Just path ->
      if path == "terms" then
        LoadPage Terms
      else if path == "privacy" then
        LoadPage Privacy
      else if path == "user" then
        LoadPage User
      else if path == "game" then
        LoadPage Game
      else if path == "settings" then
        case session of
          Api.SignedIn authSession ->
            Command <|
            Cmd.map UserSettingsMsg <|
            Api.getSettings authSession.session Pages.UserSettings.InfoLoaded
          Api.SignedOut ->
            LoadPage <| Error (Utils.StatusCode 401, "You're not signed in.")
      else if path == "game-settings" then
        LoadPage GameSettings
      else if path == "" then
        LoadPage FrontPage
      else
        LoadPage <| Error (Utils.StatusCode 404, "We don't have a page for this URL.")
    Nothing ->
      LoadPage FrontPage

type alias Model =
  { page : Page
  , key : Nav.Key
  , session : Api.Session
  , header : Base.Model
  , userSettings : Pages.UserSettings.Model
  }

removeQueryIfNeeded : Url.Url -> Nav.Key -> Cmd Msg
removeQueryIfNeeded url key =
  case url.query of
    Just path ->
      if path == "" then
        -- Remove the ? at the end because it annoys
        (Nav.replaceUrl key) <|
          Url.Builder.custom Url.Builder.Relative [ url.path ] [] url.fragment
      else
        Cmd.none
    Nothing ->
      Cmd.none

init : (Maybe String, Maybe String) -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init (sessionMaybe, usernameMaybe) url navKey =
  let
    session =
      case (sessionMaybe, usernameMaybe) of
        (Just sessionID, Just username) ->
          Api.SignedIn { session = sessionID, username = username }
        (_, _) ->
          Api.SignedOut
    (page, cmd) =
      case urlToPage url session of
        LoadPage pageType ->
          (pageType, Cmd.none)
        Command command ->
          (Loading, command)
  in
    ( { page = page
      , key = navKey
      , session = session
      , header = Base.init session
      , userSettings = Pages.UserSettings.init session
      }
    , Cmd.batch [ removeQueryIfNeeded url navKey, cmd ]
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
    Error (status, _) ->
      case status of
        Utils.ErrorStatusText text ->
          text
        Utils.StatusCode code ->
          String.fromInt code
    Loading ->
      "Loading"

content : Model -> List (Html.Html Msg)
content model =
  case model.page of
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
      List.map (Html.map UserSettingsMsg) (Pages.UserSettings.view model.session model.userSettings)
    GameSettings ->
      Pages.GameSettings.view
    Error error ->
      Pages.Error.view error
    Loading ->
      Pages.Loading.view

view : Model -> Browser.Document Msg
view model =
  { title =
    if model.page == FrontPage then
      "Elimination"
    else
      title model.page ++ " | Elimination"
  , body
    = List.map (Html.map BaseMsg) (Base.makeHeader model.session model.header)
    ++ content model
    ++ Base.makeFooter
  }

type Msg
  = ChangedUrl Url.Url
  | ClickedLink Browser.UrlRequest
  | BaseMsg Base.Msg
  | UserSettingsMsg Pages.UserSettings.Msg

port saveSession : (Api.SessionID, String) -> Cmd msg
port logout : () -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedLink request ->
      case request of
        Browser.Internal url ->
          (model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External url ->
          (model, Nav.load url)
    ChangedUrl url ->
      let
        (page, cmd) =
          case urlToPage url model.session of
            LoadPage pageType ->
              (pageType, Cmd.none)
            Command command ->
              (model.page, command)
      in
        ({ model | page = page }, Cmd.batch [ removeQueryIfNeeded url model.key, cmd ])
    BaseMsg subMsg ->
      let
        (subModel, sessionOrCmd) = Base.update subMsg model.session model.header
      in
        case sessionOrCmd of
          Api.Command cmd ->
            ({ model | header = subModel }, Cmd.map BaseMsg cmd)
          Api.ChangeSession authSession ->
            ( { model | header = subModel, session = authSession }
            , case authSession of
              Api.SignedIn { session, username } ->
                saveSession (session, username)
              Api.SignedOut ->
                logout ()
            )
    UserSettingsMsg subMsg ->
      let
        (subModel, sessionOrCmd) = Pages.UserSettings.update subMsg model.session model.userSettings
      in
        case sessionOrCmd of
          Api.Command cmd ->
            ({ model | userSettings = subModel }, Cmd.map UserSettingsMsg cmd)
          Api.ChangeSession authSession ->
            ( { model | userSettings = subModel, session = authSession }
            , case authSession of
              Api.SignedIn { session, username } ->
                saveSession (session, username)
              Api.SignedOut ->
                logout ()
            )

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.header.open /= Base.None then
    Browser.Events.onClick (D.succeed (BaseMsg Base.Close))
  else
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
