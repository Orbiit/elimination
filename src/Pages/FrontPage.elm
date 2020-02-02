module Pages.FrontPage exposing (frontPage)

import Html exposing (..)
import Html.Attributes exposing (..)

frontPage : () -> List (Html msg)
frontPage _ =
  [ div [ class "main content text" ]
    [ h1 []
      [ text "Front page" ]
    , p []
      [ text "I will add the front page later" ]
    ]
  ]
