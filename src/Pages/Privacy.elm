module Pages.Privacy exposing (privacyPage)

import Html exposing (..)
import Html.Attributes exposing (..)

import Utils exposing (extLink)

privacyPage : () -> List (Html msg)
privacyPage _ =
  [ div [ class "content text-content" ]
    [ h1 []
      [ text "Privacy policy" ]
    , p []
      [ text "Our services collect and store all user data you give us to remember that games and users exist. Examples of data collected include, but are not limited to:" ]
    , ul []
      [ li []
        [ text "Usernames" ]
      , li []
        [ text "Account emails" ]
      , li []
        [ text "Display names" ]
      , li []
        [ text "User-submitted bios" ]
      , li []
        [ text "The games you created" ]
      , li []
        [ text "The games you joined" ]
      , li []
        [ text "Game names" ]
      , li []
        [ text "User-submitted game descriptions" ]
      , li []
        [ text "Game passphrases (not encrypted)" ]
      , li []
        [ text "Whether you were killed in a game" ]
      ]
    , p []
      [ text "We also collect user passwords, but we "
      , extLink "salt" "https://en.wikipedia.org/wiki/Salt" "link"
      , text " and encrypt them before storing them. Game passphrases are NOT encrypted so that game creators can return to their game settings page to see what it is."
      ]
    , p []
      [ text "Only usernames, user display names, user bios, game names, game descriptions, the games you created and joined, and other game-related personal information are publicized." ]
    , p []
      [ text "User information is stored in an unencrypted "
      , extLink "lowdb" "https://github.com/typicode/lowdb" "link"
      , text " database without backups on a laptop. This means that someone with access to our database will be able to access a few other data such as everyone's encrypted passwords. We are not responsible for spontaneous data loss. We do not sell your user information because we don't know to whom it should be sold."
      ]
    , p []
      [ text "If you would like to download or delete all your data, please email "
      , a [ href "mailto:sy24484@pausd.us", class "link" ]
        [ text "sy24484@pausd.us" ]
      , text ". This is not because we want to discourage users from doing this; rather, we have not gotten to implementing an automatic and more user-friendly method yet."
      ]
    ]
  ]
