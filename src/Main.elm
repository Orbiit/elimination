port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Html
import Url
import Url.Builder
import Json.Decode as D
import Time
import Task

import Base
import Api
import Utils.Request as Request
import Pages
import Pages.FrontPage
import Pages.Terms
import Pages.Privacy
import Pages.About
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
  | SwitchPageAndCommand Pages.Page (Cmd Msg)

loadFrontPage : Api.GlobalModel m -> PageCmd
loadFrontPage global =
  case global.session of
    Api.SignedIn _ ->
      Command <|
        Cmd.batch
          [ Api.statuses global Pages.FrontPage.StatusesLoaded
            |> Cmd.map FrontPageMsg
          , NProgress.start ()
          ]
    Api.SignedOut ->
      Command <|
        Cmd.batch
          [ Api.getStats global Pages.FrontPage.StatsLoaded
            |> Cmd.map FrontPageMsg
          , NProgress.start ()
          ]

urlToPage : Api.GlobalModel m -> Url.Url -> PageCmd
urlToPage global url =
  case url.query of
    Just path ->
      if String.startsWith "@" path then
        let
          username = String.dropLeft 1 path
        in
          Command <|
            Cmd.batch
              [ Api.getUser global (Pages.User.InfoLoaded username) username
                |> Cmd.map UserMsg
              , NProgress.start ()
              ]
      else if String.startsWith "!" path then
        let
          gameID = String.dropLeft 1 path
        in
        Command <|
          Cmd.batch
            [ Api.getGame global (Pages.Game.InfoLoaded gameID) gameID
              |> Cmd.map GameMsg
            , NProgress.start ()
            ]
      else if path == "terms" then
        SwitchPage Pages.Terms
      else if path == "privacy" then
        SwitchPage Pages.Privacy
      else if path == "about" then
        SwitchPage Pages.About
      else if path == "game" then
        SwitchPage Pages.Game
      else if path == "settings" then
        Command <|
          Cmd.batch
            [ Cmd.map UserSettingsMsg <|
              Api.getSettings global Pages.UserSettings.InfoLoaded
            , NProgress.start ()
            ]
      else if path == "create-game" then
        SwitchPageAndCommand Pages.GameSettings (Cmd.map GameSettingsMsg Pages.GameSettings.resizeDesc)
      else if String.startsWith "settings!" path then
        let
          game = String.dropLeft (String.length "settings!") path
        in
        Command <|
          Cmd.batch
            [ Api.getGameSettings global (Pages.GameSettings.InfoLoaded game) game
              |> Cmd.map GameSettingsMsg
            , NProgress.start ()
            ]
      else if path == "" then
        loadFrontPage global
      else
        SwitchPage (Pages.Error (Request.StatusCode 404, "We don't have a page for this URL."))
    Nothing ->
      loadFrontPage global

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
  , zone : Time.Zone
  , session : Api.Session
  , host : String
  , askDiscardChanges : Maybe Browser.UrlRequest
  , header : Base.Model
  , userSettings : Pages.UserSettings.Model
  , user : Pages.User.Model
  , frontPage : Pages.FrontPage.Model
  , gameSettings : Pages.GameSettings.Model
  , game : Pages.Game.Model
  }

init : (String, Maybe String, Maybe String) -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init (host, sessionMaybe, usernameMaybe) url navKey =
  let
    session =
      case (sessionMaybe, usernameMaybe) of
        (Just sessionID, Just username) ->
          Api.SignedIn { session = sessionID, username = username }
        (_, _) ->
          Api.SignedOut
    semiGlobal =
      { session = session, zone = Time.utc, host = host }
    (page, cmd) =
      case urlToPage semiGlobal url of
        SwitchPage pageType ->
          (pageType, Cmd.none)
        Command command ->
          (Pages.Loading, command)
        SwitchPageAndCommand pageType command ->
          (pageType, command)
    (header, headerCmd) = Base.init semiGlobal
  in
    ( { page = page
      , key = navKey
      , zone = Time.utc
      , session = session
      , host = host
      , askDiscardChanges = Nothing
      , header = header
      , userSettings = Pages.UserSettings.init
      , user = Pages.User.init
      , frontPage = Pages.FrontPage.init
      , gameSettings = Pages.GameSettings.init
      , game = Pages.Game.init
      }
    , Cmd.batch
      [ removeQueryIfNeeded url navKey
      , cmd
      , Cmd.map BaseMsg headerCmd
      , Task.perform AdjustTimeZone Time.here
      ]
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
    Pages.About ->
      "About"
    Pages.User ->
      model.user.username
    Pages.Game ->
      model.game.info.name
    Pages.UserSettings ->
      "Settings"
    Pages.GameSettings ->
      if model.gameSettings.game == Nothing then "Create a game" else "Game settings"
    Pages.Error (status, _) ->
      case status of
        Request.ErrorStatusText text ->
          text
        Request.StatusCode code ->
          String.fromInt code
    Pages.Loading ->
      "Loading"

content : Model -> List (Html.Html Msg)
content model =
  case model.page of
    Pages.FrontPage ->
      Pages.FrontPage.view model model.frontPage
        |> List.map (Html.map FrontPageMsg)
    Pages.Terms ->
      Pages.Terms.view
    Pages.Privacy ->
      Pages.Privacy.view
    Pages.About ->
      Pages.About.view
    Pages.User ->
      Pages.User.view model model.user
        |> List.map (Html.map UserMsg)
    Pages.Game ->
      Pages.Game.view model model.game
        |> List.map (Html.map GameMsg)
    Pages.UserSettings ->
      Pages.UserSettings.view model model.userSettings
        |> List.map (Html.map UserSettingsMsg)
    Pages.GameSettings ->
      Pages.GameSettings.view model model.gameSettings
        |> List.map (Html.map GameSettingsMsg)
    Pages.Error error ->
      Pages.Error.view error
    Pages.Loading ->
      Pages.Loading.view

view : Model -> Browser.Document Msg
view model =
  { title =
    let
      unread = model.header.notifs.unread
      append =
        (if unsavedChanges model then "*" else "") ++
          if unread == 0 then
            ""
          else
            " (" ++ String.fromInt unread ++ ")"
    in
    if model.page == Pages.FrontPage then
      "Elimination" ++ append
    else
      title model ++ append ++ " | Elimination"
  , body =
    List.concat
      [ Base.makeHeader model model.header (model.page == Pages.FrontPage)
        |> List.map (Html.map BaseMsg)
      , content model
      , case model.askDiscardChanges of
        Just request ->
          Base.makeConfirmLeave (ClickedLink False request) CloseConfirmLeave
        Nothing ->
          []
      , Base.makeFooter
      ]
  }

type Msg
  = ClickedLink Bool Browser.UrlRequest
  | ChangedUrl Url.Url
  | CloseConfirmLeave
  | AdjustTimeZone Time.Zone
  | BaseMsg Base.Msg
  | UserSettingsMsg Pages.UserSettings.Msg
  | UserMsg Pages.User.Msg
  | FrontPageMsg Pages.FrontPage.Msg
  | GameSettingsMsg Pages.GameSettings.Msg
  | GameMsg Pages.Game.Msg
  | BeforeUnload ()
  | CheckNotifs Time.Posix
  | OnKeyDown String
  | DoNothing

port saveSession : (Api.SessionID, String) -> Cmd msg
port logout : () -> Cmd msg

port onBeforeUnload : (() -> msg) -> Sub msg
port preventUnload : () -> Cmd msg

unsavedChanges : Model -> Bool
unsavedChanges model =
  Pages.UserSettings.hasUnsavedChanges model.userSettings ||
    Pages.GameSettings.hasUnsavedChanges model.gameSettings

discardChanges : Model -> Model
discardChanges model =
  { model
  | userSettings = Pages.UserSettings.discardChanges model.userSettings
  , gameSettings = Pages.GameSettings.discardChanges model.gameSettings
  }

doPageCmd : Api.PageCmd -> (Model, Cmd Msg) -> (Model, Cmd Msg)
doPageCmd pageCmd (model, cmd) =
  case pageCmd of
    Api.ChangeSession sessionType ->
      let
        newModel =
          { model
          | session = sessionType
          -- Clear notifications
          , header = Base.clearNotifs model.header
          -- Reset front page state to clear statuses
          , frontPage = Pages.FrontPage.init
          }
      in
      ( newModel
      , case sessionType of
        Api.SignedIn { session, username } ->
          Cmd.batch
            [ saveSession (session, username)
            , case model.page of
                Pages.Error _ ->
                  Nav.pushUrl model.key "?"
                Pages.FrontPage ->
                  -- Force "reload" to refetch statuses
                  Nav.replaceUrl model.key "?"
                _ ->
                  Cmd.none
            , cmd
            , Cmd.map BaseMsg (Base.updateNotifs newModel)
            ]
        Api.SignedOut ->
          Cmd.batch
            [ logout ()
            , cmd
            ]
      )
    Api.ChangePage page ->
      ({ model | page = page }, cmd)
    Api.Redirect url ->
      (model, Cmd.batch [ Nav.pushUrl model.key url, cmd ])
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
    ClickedLink checkChanges request ->
      if checkChanges && unsavedChanges model then
        ( { model | askDiscardChanges = Just request }
        , Task.attempt (\_ -> DoNothing) (Dom.focus "confirm-leave-btn")
        )
      else
        let
          newModel =
            if checkChanges then
              model
            else
              discardChanges { model | askDiscardChanges = Nothing }
        in
        case request of
          Browser.Internal url ->
            (newModel, Nav.pushUrl model.key (Url.toString url) )
          Browser.External url ->
            (newModel, Nav.load url)
    ChangedUrl url ->
      let
        (newModel, cmd) =
          case urlToPage model url of
            SwitchPage pageType ->
              ({ model | page = pageType }, Cmd.none)
            Command command ->
              (model, command)
            SwitchPageAndCommand pageType command ->
              -- Hacky special case for creating a game
              ({ model | page = pageType, gameSettings = Pages.GameSettings.init }, command)
      in
        (newModel, Cmd.batch [ removeQueryIfNeeded url model.key, cmd ])
    CloseConfirmLeave ->
      ({ model | askDiscardChanges = Nothing }, Cmd.none)
    AdjustTimeZone zone ->
      ({ model | zone = zone }, Cmd.none)
    BaseMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Base.update subMsg model model.header
      in
        doPageCmd pageCmd ({ model | header = subModel }, Cmd.map BaseMsg subCmd)
    UserSettingsMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.UserSettings.update subMsg model model.userSettings
      in
        doPageCmd pageCmd ({ model | userSettings = subModel }, Cmd.map UserSettingsMsg subCmd)
    UserMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.User.update subMsg model model.user
      in
        doPageCmd pageCmd ({ model | user = subModel }, Cmd.map UserMsg subCmd)
    FrontPageMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.FrontPage.update subMsg model model.frontPage
      in
        doPageCmd pageCmd ({ model | frontPage = subModel }, Cmd.map FrontPageMsg subCmd)
    GameSettingsMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.GameSettings.update subMsg model model.gameSettings
      in
        doPageCmd pageCmd ({ model | gameSettings = subModel }, Cmd.map GameSettingsMsg subCmd)
    GameMsg subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.Game.update subMsg model model.game
      in
        doPageCmd pageCmd ({ model | game = subModel }, Cmd.map GameMsg subCmd)
    BeforeUnload _ ->
      (model, if unsavedChanges model then preventUnload () else Cmd.none)
    CheckNotifs _ ->
      (model, Cmd.map BaseMsg (Base.updateNotifs model))
    OnKeyDown key ->
      case key of
        "Escape" ->
          let
            (header, _, _) = Base.update Base.Close model model.header
            (frontPage, _, _) = Pages.FrontPage.update Pages.FrontPage.HideModal model model.frontPage
            (gameSettings, _, _) = Pages.GameSettings.update Pages.GameSettings.HideModal model model.gameSettings
            (game, _, _) = Pages.Game.update Pages.Game.HideModal model model.game
          in
          ( { model
            | askDiscardChanges = Nothing
            , header = header
            , frontPage = frontPage
            , gameSettings = gameSettings
            , game = game
            }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)
    DoNothing ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.header.open /= Base.None then
        Events.onClick (D.succeed (BaseMsg Base.Close))
      else
        Sub.none
    , onBeforeUnload BeforeUnload
    , Events.onKeyDown (D.map OnKeyDown (D.field "key" D.string))
    -- Check notifications every five minutes (same rate as Scratch)
    , Time.every 300000 CheckNotifs
    ]

main : Program (String, Maybe String, Maybe String) Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink True
    }
