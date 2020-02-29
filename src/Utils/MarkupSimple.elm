module Utils.MarkupSimple exposing (markup)

import Char
import Html exposing (Html, a, text)
import Html.Attributes as A
import Parser exposing (..)


type Item
    = Text String
    | Link String


urlFleshOk : List Char
urlFleshOk =
    String.toList "-+@#/%?=~_|!:,.;&"


urlFlesh : Char -> Bool
urlFlesh c =
    Char.isAlpha c || Char.isDigit c || List.member c urlFleshOk


urlEndOk : List Char
urlEndOk =
    String.toList "-#/=_"


urlEnd : Char -> Bool
urlEnd c =
    Char.isAlpha c || Char.isDigit c || List.member c urlEndOk


plainURL : Parser (Html msg)
plainURL =
    map (\chunk -> Html.a [ A.class "link" ] [ text chunk ]) <|
        getChompedString <|
            succeed ()
                |. symbol "http"
                |. oneOf
                    [ symbol "s"
                    , succeed ()
                    ]
                |. symbol "://"
                |. chompWhile urlFlesh
                |. chompIf urlEnd


markupLoop : List (Html msg) -> Parser (Step (List (Html msg)) (List (Html msg)))
markupLoop reverseHtmls =
    oneOf
        [ succeed (\html -> Loop (html :: reverseHtmls))
            |= plainURL
        , end
            |> map (\_ -> Done (List.reverse reverseHtmls))
        , chompWhile (\c -> c /= 'h')
            |> getChompedString
            |> map (\chunk -> Loop (text chunk :: reverseHtmls))
        ]


stringHelp : List (Html msg) -> Parser (Step (List (Html msg)) (List (Html msg)))
stringHelp revChunks =
    oneOf
        [ map (\chunk -> Loop (Html.a [ A.class "link" ] [ text chunk ] :: revChunks)) <|
            getChompedString <|
                -- backtrackable <|
                succeed ()
                    -- \chunk -> Loop (chunk :: revChunks)
                    |. token "h"
                    |. oneOf
                        [ token "ttp"
                            |. oneOf
                                [ symbol "s"
                                , succeed ()
                                ]
                        , succeed ()
                        ]

        -- |= map (\_ -> a [ A.class "link", A.href "?" ] [text "TI"]) (token "ttp")
        , end
            |> map (\_ -> Done (List.reverse revChunks))
        , chompWhile (\c -> c /= 'h')
            |> getChompedString
            |> map (\chunk -> Loop (text chunk :: revChunks))
        ]


markupParser : Parser (List (Html msg))
markupParser =
    loop [] stringHelp


markup : String -> List (Html msg)
markup input =
    case Debug.log "output" <| run markupParser (Debug.log "input" input) of
        Ok html ->
            html

        Err _ ->
            [ text input ]
