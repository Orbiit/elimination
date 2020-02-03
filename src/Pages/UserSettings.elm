module Pages.UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes as A

import Api

type alias Model =
  { }

init : Api.Session -> Model
init _ =
  {}

type Msg
  = Dance

update : Msg -> Api.Session -> Model -> (Model, Cmd Msg)
update msg session model =
  (model, Cmd.none)

view : Api.Session -> Model -> List (Html msg)
view session model =
  [ div [ A.class "main content settings" ]
    [ h1 []
      [ text "User settings"
      , span [ A.class "flex" ]
        []
      , a [ A.class "button", A.href "./user.html" ]
        [ text "View profile" ]
      , button [ A.class "button" ]
        [ text "Sign out" ]
      ]
    , form []
      [ div [ A.class "input-row" ]
        [ label [ A.class "input-wrapper" ]
          [ span [ A.class "label" ]
            [ text "Display name" ]
          , div [ A.class "input" ]
            [ input [ A.name "name", A.placeholder "Billy Chelontuvier", A.attribute "required" "", A.type_ "text", A.value "Leuf Munkler" ]
              []
            , span [ A.class "count" ]
              [ text "13/50" ]
            ]
          , span [ A.class "sublabel" ]
            [ text "This lets others be able to find and eliminate you, which makes the game fair." ]
          ]
        , label [ A.class "input-wrapper" ]
          [ span [ A.class "label" ]
            [ text "Email" ]
          , div [ A.class "input" ]
            [ input [ A.name "email", A.placeholder "billygamer5@example.com", A.attribute "required" "", A.type_ "email", A.value "lmunkler@example.com" ]
              []
            ]
          , span [ A.class "sublabel" ]
            [ text "We will send password reset forms (and notifications if enabled) to this email." ]
          ]
        ]
      , div [ A.class "input-row" ]
        [ label [ A.class "input-wrapper" ]
          [ span [ A.class "label" ]
            [ text "Bio" ]
          , div [ A.class "input" ]
            [ textarea [ A.name "bio", A.placeholder "Introduce yourself here", A.attribute "required" "" ]
              [ text "Hello. I pride myself in my many achievements in science and technology, and I hope to share them with you today in my A.class. I often teach science and physics, but due to limited funding in our department, I am only able to teach the latter half of physics. This includes kinematic motion and angular velocity, among other exciting concepts. I treat all my students with excessive love and respect, and my students often reciprocate similarly." ]
            , span [ A.class "count" ]
              [ text "11/1000" ]
            ]
          ]
        ]
      , h2 []
        [ text "Change your password" ]
      , div [ A.class "input-row" ]
        [ label [ A.class "input-wrapper error" ]
          [ span [ A.class "label" ]
            [ text "New password" ]
          , div [ A.class "input" ]
            [ input [ A.name "password", A.placeholder "hunter2", A.type_ "password" ]
              []
            ]
          , span [ A.class "problem" ]
            [ text "Password too short." ]
          , span [ A.class "sublabel" ]
            [ text "Must be at least 3 poop emoji long." ]
          ]
        , label [ A.class "input-wrapper" ]
          [ span [ A.class "label" ]
            [ text "Old password" ]
          , div [ A.class "input" ]
            [ input [ A.name "oldPassword", A.placeholder "hunter2", A.type_ "password" ]
              []
            ]
          ]
        ]
      , input [ A.class "button submit-btn", A.attribute "disabled" "", A.type_ "submit", A.value "Save" ]
        []
      ]
    ]
  ]
