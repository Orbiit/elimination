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
import Pages
import Pages.FrontPage
import Pages.Terms
import Pages.Privacy
import Pages.User
import Pages.Game
import Pages.UserSettings
import Pages.GameSettings
import Pages.Loading
import Pages.Error
import NProgress

type PageCmd
  = SwitchPage Pages.Page
  | Command (Cmd Msg)

loadFrontPage : Api.Session -> PageCmd
loadFrontPage session =
  case session of
    Api.SignedIn authSession ->
      Command <|
        Cmd.batch
          [ Cmd.map FrontPageMsg <|
            Api.statuses authSession.session Pages.FrontPage.StatusesLoaded
          , NProgress.start ()
          ]
    Api.SignedOut ->
      Command <|
        Cmd.batch
          [ Cmd.map FrontPageMsg <|
            Api.getStats Pages.FrontPage.StatsLoaded
          , NProgress.start ()
          ]

urlToPage : Url.Url -> Api.Session -> PageCmd
urlToPage url session =
  case url.query of
    Just path ->
      if String.startsWith "@" path then
        let
          username = String.dropLeft 1 path
        in
          Command <|
            Cmd.batch
              [ Cmd.map UserMsg <|
                Api.getUser username (Pages.User.InfoLoaded username)
              , NProgress.start ()
              ]
      else if path == "terms" then
        SwitchPage Pages.Terms
      else if path == "privacy" then
        SwitchPage Pages.Privacy
      else if path == "game" then
        SwitchPage Pages.Game
      else if path == "settings" then
        case session of
          Api.SignedIn authSession ->
            Command <|
              Cmd.batch
                [ Cmd.map UserSettingsMsg <|
                  Api.getSettings authSession.session Pages.UserSettings.InfoLoaded
                , NProgress.start ()
                ]
          Api.SignedOut ->
            SwitchPage (Pages.Error (Utils.StatusCode 401, "You're not signed in."))
      else if path == "create-game" then
        SwitchPage (Pages.GameSettings True)
      else if path == "game-settings" then
        SwitchPage (Pages.GameSettings False)
      else if path == "" then
        loadFrontPage session
      else
        SwitchPage (Pages.Error (Utils.StatusCode 404, "We don't have a page for this URL."))
    Nothing ->
      loadFrontPage session

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

type alias Model =
  { page : Pages.Page
  , key : Nav.Key
  , session : Api.Session
  , header : Base.Model
  , userSettings : Pages.UserSettings.Model
  , user : Pages.User.Model
  , frontPage : Pages.FrontPage.Model
  }

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
        SwitchPage pageType ->
          (pageType, Cmd.none)
        Command command ->
          (Pages.Loading, command)
  in
    ( { page = page
      , key = navKey
      , session = session
      , header = Base.init session
      , userSettings = Pages.UserSettings.init session
      , user = Pages.User.init session
      , frontPage = Pages.FrontPage.init session
      }
    , Cmd.batch [ removeQueryIfNeeded url navKey, cmd ]
    )

title : Model -> String
title model =
  case model.page of
    Pages.FrontPage ->
      ""
    Pages.Terms ->
      "Terms of use"
    Pages.Privacy ->
      "Privacy policy"
    Pages.User ->
      model.user.username
    Pages.Game ->
      "Game"
    Pages.UserSettings ->
      "Settings"
    Pages.GameSettings creating ->
      if creating then "Create a game" else "Game settings"
    Pages.Error (status, _) ->
      case status of
        Utils.ErrorStatusText text ->
          text
        Utils.StatusCode code ->
          String.fromInt code
    Pages.Loading ->
      "Loading"

content : Model -> List (Html.Html Msg)
content model =
  case model.page of
    Pages.FrontPage ->
      List.map (Html.map FrontPageMsg) (Pages.FrontPage.view model.session model.frontPage)
    Pages.Terms ->
      Pages.Terms.view
    Pages.Privacy ->
      Pages.Privacy.view
    Pages.User ->
      List.map (Html.map UserMsg) (Pages.User.view model.session model.user)
    Pages.Game ->
      Pages.Game.view
    Pages.UserSettings ->
      List.map (Html.map UserSettingsMsg) (Pages.UserSettings.view model.session model.userSettings)
    Pages.GameSettings creating ->
      Pages.GameSettings.view
    Pages.Error error ->
      Pages.Error.view error
    Pages.Loading ->
      Pages.Loading.view

view : Model -> Browser.Document Msg
view model =
  { title =
    if model.page == Pages.FrontPage then
      "Elimination"
    else
      title model ++ " | Elimination"
  , body
    = List.map (Html.map BaseMsg) (Base.makeHeader model.session model.header (model.page == Pages.FrontPage))
    ++ content model
    ++ Base.makeFooter
  }

type Msg
  = ChangedUrl Url.Url
  | ClickedLink Browser.UrlRequest
  | BaseMsg Base.Msg
  | UserSettingsMsg Pages.UserSettings.Msg
  | UserMsg Pages.User.Msg
  | FrontPageMsg Pages.FrontPage.Msg

port saveSession : (Api.SessionID, String) -> Cmd msg
port logout : () -> Cmd msg

doPageCmd : Api.PageCmd -> (Model, Cmd Msg) -> (Model, Cmd Msg)
doPageCmd pageCmd (model, cmd) =
  case pageCmd of
    Api.ChangeSession authSession ->
      ( { model
        | session = authSession
        }
      , case authSession of
        Api.SignedIn { session, username } ->
          Cmd.batch
            [ saveSession (session, username)
            , case model.page of
                Pages.Error _ ->
                  Nav.pushUrl model.key "?"
                _ ->
                  Cmd.none
            , cmd
            ]
        Api.SignedOut ->
          Cmd.batch
            [ logout ()
            , Nav.pushUrl model.key "?"
            , cmd
            ]
      )
    Api.ChangePage page ->
      ({ model | page = page }, cmd)
    Api.Batch pageCmds ->
      case pageCmds of
        firstPageCmd :: others ->
          doPageCmd (Api.Batch others) (doPageCmd firstPageCmd (model, cmd))
        [] ->
          (model, cmd)
    Api.None ->
      (model, cmd)

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
            SwitchPage pageType ->
              (pageType, Cmd.none)
            Command command ->
              (model.page, command)
      in
        ({ model | page = page }, Cmd.batch [ removeQueryIfNeeded url model.key, cmd ])
    BaseMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Base.update subMsg model.session model.header
      in
        doPageCmd pageCmd ({ model | header = subModel }, Cmd.map BaseMsg subCmd)
    UserSettingsMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.UserSettings.update subMsg model.session model.userSettings
      in
        doPageCmd pageCmd ({ model | userSettings = subModel }, Cmd.map UserSettingsMsg subCmd)
    UserMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.User.update subMsg model.session model.user
      in
        doPageCmd pageCmd ({ model | user = subModel }, Cmd.map UserMsg subCmd)
    FrontPageMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.FrontPage.update subMsg model.session model.frontPage
      in
        doPageCmd pageCmd ({ model | frontPage = subModel }, Cmd.map FrontPageMsg subCmd)

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
