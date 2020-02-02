module Base exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)

import Api
import Utils

type HeaderWindow
  = SignUp
  | Login
  | Notifications
  | None

type alias Model =
  { open : HeaderWindow
  }

init : Api.Session -> Model
init _ =
  { open = None
  }

type Msg
  = Open HeaderWindow
  | Dance

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Open window ->
      ( { model | open = window }, Cmd.none )
    _ ->
      ( model, Cmd.none )

makeHeader : Api.Session -> Model -> List (Html Msg)
makeHeader session model =
  [ header [ A.class "header" ]
    ([ a [ A.href "?", A.class "site-name link" ]
      [ text "Elimination" ]
    , span [ A.class "flex" ] []
    ]
    ++ case session of
      Api.SignedIn { username } ->
        [ div [ A.class "header-window-wrapper" ]
          [ button [ A.class "icon-btn header-btn notif-btn" ]
            [ text "Notifications" ]
          , div [ A.class "header-window notifs" ]
            [ h2 [ A.class "notif-header" ]
              [ text "Notifications"
              , span [ A.class "flex" ]
                []
              , label [ A.class "email-notifs" ]
                [ input [ A.class "checkbox", A.type_ "checkbox" ]
                  []
                , text "Send notifications to my email"
                ]
              ]
            , div [ A.class "notif" ]
              [ span [ A.class "notif-timestamp" ]
                [ text "2020-01-13 at 16:01" ]
              , span [ A.class "notif-msg" ]
                [ a [ A.class "link", A.href "./user.html" ]
                  [ text "Jame Sooth" ]
                , text "has just eliminated you in "
                , a [ A.class "link", A.href "./game.html" ]
                  [ text "Practice elimination round 6" ]
                , text "."
                ]
              ]
            , div [ A.class "notif" ]
              [ span [ A.class "notif-timestamp" ]
                [ text "2019-12-01 at 06:39" ]
              , span [ A.class "notif-msg" ]
                [ text "You were kicked from "
                , a [ A.class "link", A.href "./game.html" ]
                  [ text "Test round. DO NOT JOIN." ]
                , text "because \"boomer\"."
                ]
              ]
            ]
          ]
        , a [ A.class "link username", A.href "./user-settings.html" ]
          [ text username ]
        ]
      Api.SignedOut ->
        [ div [ A.class "header-window-wrapper", A.classList [ ("open", model.open == Login) ] ]
          [ button [ A.class "header-btn auth-btn", onClick (Open Login) ]
            [ text "Log in" ]
          , form [ A.action "./front-page.html", A.class "header-window" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Username" ]
              , div [ A.class "input" ]
                [ input [ A.name "username", A.placeholder "billygamer5", A.attribute "required" "", A.type_ "text" ]
                  []
                ]
              ]
            , label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Password" ]
              , div [ A.class "input" ]
                [ input [ A.name "password", A.placeholder "hunter2", A.attribute "required" "", A.type_ "password" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Log in" ]
              []
            ]
          ]
        , div [ A.class "header-window-wrapper" ]
          [ button [ A.class "header-btn auth-btn" ]
            [ text "Sign up" ]
          , form [ A.action "./front-page.html", A.class "header-window" ]
            [ label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Username" ]
              , div [ A.class "input" ]
                [ input [ A.name "username", A.placeholder "billygamer5", A.attribute "required" "", A.type_ "text" ]
                  []
                ]
              , span [ A.class "sublabel" ]
                [ text "Only letters, digits, underscores, and hyphens are allowed. This cannot be changed later." ]
              ]
            , label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Full name" ]
              , div [ A.class "input" ]
                [ input [ A.name "name", A.placeholder "Billy Chelontuvier", A.attribute "required" "", A.type_ "text" ]
                  []
                ]
              , span [ A.class "sublabel" ]
                [ text "Allows others to be able to find you for elimination, which makes the game fair." ]
              ]
            , label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Email" ]
              , div [ A.class "input" ]
                [ input [ A.name "email", A.placeholder "billygamer5@example.com", A.attribute "required" "", A.type_ "email" ]
                  []
                ]
              , span [ A.class "sublabel" ]
                [ text "For password reset forms. You can also turn on email notifications if you want." ]
              ]
            , label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Password" ]
              , div [ A.class "input" ]
                [ input [ A.name "password", A.placeholder "hunter2", A.attribute "required" "", A.type_ "password" ]
                  []
                ]
              , span [ A.class "sublabel" ]
                [ text "Must be at least 3 poop emoji long." ]
              ]
            , label [ A.class "input-wrapper" ]
              [ span [ A.class "label" ]
                [ text "Password again" ]
              , div [ A.class "input" ]
                [ input [ A.placeholder "hunter2", A.attribute "required" "", A.type_ "password" ]
                  []
                ]
              ]
            , input [ A.class "button submit-btn", A.type_ "submit", A.value "Sign up" ]
              []
            ]
          ]
        ])
  ]

makeFooter : List (Html msg)
makeFooter =
  [ footer [ A.class "footer" ]
    [ span []
      [ text "Created by the creators of "
      , Utils.extLink "UGWA" "https://gunn.app/" "link"
      , text "."
      ]
    , span [ A.class "flex" ] []
    , span []
      [ a [ A.href "?about", A.class "link" ]
        [ text "About" ]
      , text (" " ++ Utils.char Utils.Middot ++ " ")
      , a [ A.href "?privacy", A.class "link" ]
        [ text "Privacy policy" ]
      , text (" " ++ Utils.char Utils.Middot ++ " ")
      , a [ A.href "?terms", A.class "link" ]
        [ text "Terms of use" ]
      ]
    ]
  ]
