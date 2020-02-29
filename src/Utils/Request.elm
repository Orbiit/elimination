module Utils.Request exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


type ErrorStatus
    = ErrorStatusText String
    | StatusCode Int


type alias ErrorMessage =
    String


type alias HttpError =
    ( ErrorStatus, ErrorMessage )


parseWucky : String -> ErrorMessage
parseWucky body =
    case D.decodeString (D.oneOf [ D.field "mistake" D.string, D.field "problem" D.string ]) body of
        Ok wucky ->
            wucky

        Err parseError ->
            "Supposedly something went wrong, but the server didn't articulate well enough about it:\n"
                ++ D.errorToString parseError


parseResponse : D.Decoder a -> Http.Response String -> Result HttpError a
parseResponse decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err
                ( ErrorStatusText "Bad URL"
                , "The request made to the server was done awkwardly, so the server didn't know what to do."
                )

        Http.Timeout_ ->
            Err ( ErrorStatusText "Timeout", "The server took too long." )

        Http.NetworkError_ ->
            Err ( ErrorStatusText "Offline", "Either you or the server is offline." )

        Http.BadStatus_ metadata body ->
            Err
                ( StatusCode metadata.statusCode
                , case metadata.statusCode of
                    400 ->
                        parseWucky body

                    500 ->
                        "The server hurt itself in the process of fulfilling your request:\n"
                            ++ parseWucky body

                    404 ->
                        "The server apparently doesn't know what it's meant to do."

                    _ ->
                        "The server...????"
                )

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Ok value ->
                    Ok value

                Err parseError ->
                    Err
                        ( ErrorStatusText "Malformed JSON"
                        , "The server spoke in a different language, and we couldn't understand it.\n"
                            ++ D.errorToString parseError
                        )


type RequestMethod
    = Post
    | Get


request :
    RequestMethod
    -> String
    -> Maybe String
    -> (Result HttpError a -> msg)
    -> Maybe E.Value
    -> D.Decoder a
    -> Cmd msg
request method url session msg body decoder =
    Http.request
        { method =
            case method of
                Post ->
                    "POST"

                Get ->
                    "GET"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest" ]
                ++ (case session of
                        Just sessionID ->
                            [ Http.header "X-Session-Id" sessionID ]

                        Nothing ->
                            []
                   )
        , url = url
        , body =
            case body of
                Just jsonBody ->
                    Http.jsonBody jsonBody

                Nothing ->
                    Http.emptyBody
        , expect = Http.expectStringResponse msg (parseResponse decoder)
        , timeout = Nothing
        , tracker = Nothing
        }
