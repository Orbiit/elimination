module Base exposing (makeHeader, makeFooter)

import Html exposing (..)
import Html.Attributes exposing (..)

import Utils exposing (char, Char(..))

makeHeader : () -> List (Html msg)
makeHeader _ =
  [ header [ class "header" ]
    [ a [ href "?", class "site-name link" ]
      [ text "Elimination" ]
    , span [ class "flex" ] []
    ]
  ]

makeFooter : () -> List (Html msg)
makeFooter _ =
  [ footer [ class "footer" ]
    [ span []
      [ text "Created by the creators of "
      , a [ href "https://gunn.app/", rel "noopener noreferrer", target "_blank", class "link" ]
        [ text "UGWA" ]
      , text "."
      ]
    , span [ class "flex" ] []
    , span []
      [ a [ href "?about", class "link" ]
        [ text "About" ]
      , text (" " ++ char Middot ++ " ")
      , a [ href "?privacy", class "link" ]
        [ text "Privacy policy" ]
      , text (" " ++ char Middot ++ " ")
      , a [ href "?terms", class "link" ]
        [ text "Terms of use" ]
      ]
    ]
  ]
