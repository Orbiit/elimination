module Pages.Terms exposing (termsPage)

import Html exposing (..)
import Html.Attributes exposing (..)

termsPage : () -> List (Html msg)
termsPage _ =
  [ div [ class "content text-content" ]
    [ h1 []
      [ text "Terms of use" ]
    , p []
      [ text "Please do not do anything illegal or taboo with our service." ]
    ]
  ]
