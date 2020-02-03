module Pages.User exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

view : List (Html msg)
view =
  [ article [ class "main content profile" ]
    [ div [ class "profile-info" ]
      [ h1 [ class "profile-name" ]
        [ a [ class "link", href "./user.html" ]
          [ text "Leuf Munkler" ]
        , span [ class "flex" ]
          []
        , a [ class "icon-btn settings-btn", href "./user-settings.html" ]
          [ text "Settings" ]
        ]
      , span [ class "profile-username" ]
        [ text "leuf_munkler68" ]
      , p [ class "profile-desc" ]
        [ text "Hello. I pride myself in my many achievements in science and technology, and I hope to share them with you today in my class. I often teach science and physics, but due to limited funding in our department, I am only able to teach the latter half of physics. This includes kinematic motion and angular velocity, among other exciting concepts. I treat all my students with excessive love and respect, and my students often reciprocate similarly." ]
      , p [ class "profile-desc profile-stats" ]
        [ text "Total eliminations: 22" ]
      ]
    , div [ class "lists" ]
      [ section [ class "list" ]
        [ h2 []
          [ text "Games I created" ]
        , a [ class "item", href "./game.html" ]
          [ span [ class "item-name" ]
            [ text "Mr. Munkler's E period knife practice" ]
          , span [ class "item-info" ]
            [ text "24 participants · Ended" ]
          ]
        ]
      , section [ class "list" ]
        [ h2 []
          [ text "Games in which I participate" ]
        , a [ class "item", href "./game.html" ]
          [ span [ class "item-name" ]
            [ text "Pistole High School Elimination 2020" ]
          , span [ class "item-info" ]
            [ text "302 participants · 16 eliminations · Alive · Ongoing" ]
          ]
        , a [ class "item", href "./game.html" ]
          [ span [ class "item-name" ]
            [ text "Pistole High School eliminations 2017" ]
          , span [ class "item-info" ]
            [ text "278 participants · 0 eliminations · Eliminated · Ended" ]
          ]
        , a [ class "item", href "./game.html" ]
          [ span [ class "item-name" ]
            [ text "Pistole HS Elimination 2016" ]
          , span [ class "item-info" ]
            [ text "230 participants · 6 eliminations · Eliminated · Ended" ]
          ]
        ]
      ]
    ]
  ]