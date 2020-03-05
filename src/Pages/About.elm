module Pages.About exposing (view)

import Html exposing (..)
import Html.Attributes as A
import Utils

view : List (Html msg)
view =
  [ div [ A.class "main content text" ]
    [ h1 []
      [ text "About and help" ]
    , p []
      [ text "If you have any questions or something isn't working, you can email "
      , a [ A.class "link", A.href "mailto:sy24484@pausd.us" ]
        [ text "sy24484@pausd.us" ]
      , text " or send a message to "
      , Utils.extLink "Ovinus Real" "https://www.facebook.com/ovinus.genuine" "link"
      , text " on Facebook."
      ]
    , h2 []
      [ text "How do I join?" ]
    , p []
      [ text "You're probably looking for "
      , a [ A.class "link", A.href "?!bbdd6" ]
        [ text "Gunn Elimination 2020" ]
      , text " if you're a Gunn student."
      ]
    , p []
      [ text "Once you find the game page, click on the \"Join\" button on the top right (make sure you're logged in first) and enter in the passphrase. If you want to leave, you can click the \"Leave\" button on the top right." ]
    , h2 [ A.id "elimination-sequences" ]
      [ text "What are elimination sequences?" ]
    , p []
      [ text "When a game starts, each player is assigned an elimination sequence and a target. The player must locate their target and \"eliminate\" them according to the game's rules. Then, the target is obligated to reveal their elimination sequence to the player. On the website, the player clicks the \"Eliminate\" button and enters in their target's elimination sequence to eliminate them, and the website will assign them a new target to find." ]
    , h2 [ A.id "formatting" ]
      [ text "Formatting" ]
    , p []
      [ text "User bios and game descriptions support basic formatting. The following" ]
    , blockquote []
      [ p []
        [ text "Mention a user: @example\nMention a game: !28a5b\nPost links: https://gunn.app/\n\n**Bold**, *italics*, ***both bold and italics***. Escaping characters: \\* lmunkler\\@example.com" ]
      ]
    , p []
      [ text "produces" ]
    , blockquote []
      [ p []
        [ text "Mention a user: "
        , a [ A.class "link", A.href "?@example" ]
          [ text "@example" ]
        , text "\nMention a game: "
        , a [ A.class "link", A.href "?!28a5b" ]
          [ text "Example game" ]
        , text "\nPost links: "
        , Utils.extLink "https://gunn.app/" "https://gunn.app/" "link"
        , text "\n\n"
        , strong []
          [ text "Bold" ]
        , text ", "
        , em []
          [ text "italics" ]
        , text ", "
        , strong []
          [ em []
            [ text "both bold and italics" ]
          ]
        , text ". Escaping characters: * lmunkler@example.com"
        ]
      ]
    ]
  ]
