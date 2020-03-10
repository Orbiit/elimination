port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Html
import Url exposing (Url)
import Url.Builder
import Json.Decode as D
import Time
import Task

import Base
import Api
import Utils
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
import Pages.ResetPassword
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

urlToPage : Api.GlobalModel m -> Url -> PageCmd
urlToPage global url =
  case url.query of
    Just query ->
      let
        path = case List.head (String.split "&" query) of
          Just str ->
            str
          Nothing ->
            ""
      in
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
      else if String.startsWith "reset-" path then
        SwitchPage (Pages.ResetPassword (String.dropLeft (String.length "reset-") path))
      -- `=` probably means there are trackers in the URL
      else if path == "" || String.contains "=" path then
        loadFrontPage global
      else
        SwitchPage (Pages.Error (Request.StatusCode 404, "We don't have a page for this URL."))
    Nothing ->
      loadFrontPage global

shouldRemoveQuery : Url -> Bool
shouldRemoveQuery url =
  case url.query of
    Just path -> path == ""
    Nothing -> False

-- Remove the ? at the end because it annoys
removeQuery : Url -> Nav.Key -> Cmd msg
removeQuery url key =
  Url.Builder.custom Url.Builder.Relative [ url.path ] [] url.fragment
    |> Nav.replaceUrl key

type ConfirmDiscard
  = Hidden
  | LeaveTo Browser.UrlRequest
  | SignOut

type alias Model =
  { page : Pages.Page
  , url : Url
  , key : Nav.Key
  , session : Api.Session
  , host : String
  , askDiscardChanges : ConfirmDiscard
  , header : Base.Model
  , userSettings : Pages.UserSettings.Model
  , user : Pages.User.Model
  , frontPage : Pages.FrontPage.Model
  , gameSettings : Pages.GameSettings.Model
  , game : Pages.Game.Model
  , resetPassword : Pages.ResetPassword.Model
  }

init : (String, Maybe String, Maybe String) -> Url -> Nav.Key -> (Model, Cmd Msg)
init (host, sessionMaybe, usernameMaybe) url navKey =
  let
    session =
      case (sessionMaybe, usernameMaybe) of
        (Just sessionID, Just username) ->
          Api.SignedIn { session = sessionID, username = username }
        (_, _) ->
          Api.SignedOut
    semiGlobal =
      { session = session, host = host }
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
      , url = url
      , key = navKey
      , session = session
      , host = host
      , askDiscardChanges = Hidden
      , header = header
      , userSettings = Pages.UserSettings.init
      , user = Pages.User.init
      , frontPage = Pages.FrontPage.init
      , gameSettings = Pages.GameSettings.init
      , game = Pages.Game.init
      , resetPassword = Pages.ResetPassword.init
      }
    , Cmd.batch
      [ if shouldRemoveQuery url then
          removeQuery url navKey
        else
          cmd
      , Cmd.map BaseMsg headerCmd
      , Utils.scrollIfNeeded DoNothing url.fragment
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
    Pages.ResetPassword _ ->
      "Reset password"
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
    Pages.ResetPassword id ->
      Pages.ResetPassword.view model model.resetPassword
        |> List.map (Html.map (ResetPasswordMsg id))
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
      [ Base.makeMaintenanceWarning
      , Base.makeHeader model model.header (model.page == Pages.FrontPage)
        |> List.map (Html.map BaseMsg)
      , content model
      , case model.askDiscardChanges of
        LeaveTo request ->
          Base.makeConfirmLeave (ClickedLink False request) CloseConfirmLeave
        SignOut ->
          Base.makeConfirmSignOut SigningOut CloseConfirmLeave
        Hidden ->
          []
      , Base.makeFooter
      ]
  }

type Msg
  = ClickedLink Bool Browser.UrlRequest
  | ChangedUrl Url
  | CloseConfirmLeave
  | BaseMsg Base.Msg
  | UserSettingsMsg Pages.UserSettings.Msg
  | UserMsg Pages.User.Msg
  | FrontPageMsg Pages.FrontPage.Msg
  | GameSettingsMsg Pages.GameSettings.Msg
  | GameMsg Pages.Game.Msg
  | ResetPasswordMsg String Pages.ResetPassword.Msg
  | BeforeUnload ()
  | CheckNotifs Time.Posix
  | OnKeyDown String
  | SigningOut
  | SignedOut (Api.Response ())
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

updateFromUrl : Model -> Url -> (Model, Cmd Msg)
updateFromUrl model url =
  case urlToPage model url of
    SwitchPage pageType ->
      ( { model | page = pageType }
      , Utils.scrollIfNeeded DoNothing url.fragment
      )
    Command command ->
      (model, command)
    SwitchPageAndCommand pageType command ->
      -- Hacky special case for creating a game
      ({ model | page = pageType, gameSettings = Pages.GameSettings.init }, command)

pageOnSessionChange : Model -> (Model, Cmd Msg)
pageOnSessionChange model =
  case model.page of
    Pages.FrontPage -> updateFromUrl model model.url
    Pages.Error _ -> updateFromUrl model model.url
    Pages.UserSettings ->
      case model.session of
        Api.SignedIn _ -> updateFromUrl model model.url
        Api.SignedOut -> (model, Nav.pushUrl model.key "?")
    Pages.GameSettings ->
      case (model.session, model.gameSettings.game) of
        (Api.SignedOut, Just gameID) ->
          (model, Nav.pushUrl model.key ("?!" ++ gameID))
        (Api.SignedOut, Nothing) ->
          (model, Nav.pushUrl model.key "?")
        (Api.SignedIn _, _) ->
          updateFromUrl model model.url
    _ ->
      (model, Cmd.none)

doPageCmd : Api.PageCmd -> (Model, Cmd Msg) -> (Model, Cmd Msg)
doPageCmd pageCmd (model, cmd) =
  case pageCmd of
    Api.SignIn sessionID username ->
      let
        newModel =
          { model
          | session = Api.SignedIn { session = sessionID, username = username}
          -- Clear notifications
          , header = Base.clearNotifs model.header
          -- Reset front page state to clear statuses
          , frontPage = Pages.FrontPage.clearStatus model.frontPage
          }
        (newModel2, cmdFromPage) = pageOnSessionChange newModel
      in
      ( newModel2
      , Cmd.batch
        [ saveSession (sessionID, username)
        , cmdFromPage
        , cmd
        , Cmd.map BaseMsg (Base.updateNotifs newModel)
        ]
      )
    Api.AttemptSignOut ->
      if model.session == Api.SignedOut then
        (model, cmd)
      else if unsavedChanges model then
        ({ model | askDiscardChanges = SignOut }, cmd)
      else
        let
          (newModel, extraCmd) = update SigningOut model
        in
        (newModel, Cmd.batch [ extraCmd, cmd ])
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
        ( { model | askDiscardChanges = LeaveTo request }
        , Task.attempt (\_ -> DoNothing) (Dom.focus "confirm-leave-btn")
        )
      else
        let
          newModel =
            if checkChanges then
              model
            else
              discardChanges { model | askDiscardChanges = Hidden }
        in
        case request of
          Browser.Internal url ->
            (newModel, Nav.pushUrl model.key (Url.toString url) )
          Browser.External url ->
            (newModel, Nav.load url)
    ChangedUrl url ->
      if shouldRemoveQuery url then
        ({ model | url = url }, removeQuery url model.key)
      else
        let
          (newModel, cmd) = updateFromUrl model url
        in
        ({ newModel | url = url }, cmd)
    CloseConfirmLeave ->
      ({ model | askDiscardChanges = Hidden }, Cmd.none)
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
    ResetPasswordMsg id subMsg ->
      let
        (subModel, subCmd, pageCmd) = Pages.ResetPassword.update subMsg model model.resetPassword id
      in
      doPageCmd pageCmd ({ model | resetPassword = subModel }, Cmd.map (ResetPasswordMsg id) subCmd)
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
            | askDiscardChanges = Hidden
            , header = header
            , frontPage = frontPage
            , gameSettings = gameSettings
            , game = game
            }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)
    SigningOut ->
      let
        (newModel, cmd) = pageOnSessionChange <|
          discardChanges { model | session = Api.SignedOut, askDiscardChanges = Hidden }
      in
      (newModel, Cmd.batch [ logout (), Api.logout model SignedOut, cmd ])
    SignedOut _ ->
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
