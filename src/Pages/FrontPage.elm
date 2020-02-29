module Pages.FrontPage exposing (..)

import Api
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onSubmit, stopPropagationOn)
import Json.Decode as D
import NProgress
import Pages
import Task
import Utils exposing (Char(..), char)
import Utils.Input as Input exposing (myInputDefaults)


type alias Model =
    { stats : Api.Stats
    , statuses : List Api.Status
    , other : List Api.OtherGame
    , modal : Maybe Api.GameID
    , code : String
    , killing : Bool
    , problem : Maybe String
    , showingCode : Maybe Api.GameID
    }


init : Model
init =
    { stats =
        { kills = 0
        , active = 0
        , games = 0
        }
    , statuses = []
    , other = []
    , modal = Nothing
    , code = ""
    , killing = False
    , problem = Nothing
    , showingCode = Nothing
    }


type Msg
    = StatsLoaded (Api.Response Api.Stats)
    | StatusesLoaded (Api.Response Api.GameStatuses)
    | ShowModal Api.GameID
    | HideModal
    | ChangeCode Input.MyInputMsg
    | Kill
    | Killed (Api.Response ())
    | ShowCode Api.GameID
    | DoNothing


update : Msg -> Api.GlobalModel m -> Model -> ( Model, Cmd Msg, Api.PageCmd )
update msg global model =
    case msg of
        StatsLoaded result ->
            case result of
                Ok stats ->
                    ( { model | stats = stats }, NProgress.done (), Api.ChangePage Pages.FrontPage )

                Err error ->
                    ( model, NProgress.done (), Api.ChangePage (Pages.Error error) )

        StatusesLoaded result ->
            case result of
                Ok { statuses, other } ->
                    ( { model | statuses = statuses, other = other, showingCode = Nothing }
                    , NProgress.done ()
                    , Api.ChangePage Pages.FrontPage
                    )

                Err (( _, errorMsg ) as error) ->
                    ( model
                    , NProgress.done ()
                    , Api.Batch [ Api.ChangePage (Pages.Error error), Api.sessionCouldExpire error ]
                    )

        ShowModal game ->
            ( { model | modal = Just game, code = "", problem = Nothing }
            , Task.attempt (\_ -> DoNothing) (Dom.focus "kill-modal-input")
            , Api.None
            )

        HideModal ->
            ( { model | modal = Nothing }, Cmd.none, Api.None )

        ChangeCode { value } ->
            ( { model | code = value }, Cmd.none, Api.None )

        Kill ->
            case model.modal of
                Just game ->
                    ( { model | problem = Nothing, killing = True }
                    , Api.kill global Killed game model.code
                    , Api.None
                    )

                Nothing ->
                    ( model, Cmd.none, Api.None )

        Killed result ->
            case result of
                Ok _ ->
                    ( { model | killing = False, modal = Nothing }
                    , Cmd.batch
                        [ Api.statuses global StatusesLoaded
                        , NProgress.start ()
                        ]
                    , Api.None
                    )

                Err (( _, errorMsg ) as error) ->
                    ( { model | killing = False, problem = Just errorMsg }, Cmd.none, Api.sessionCouldExpire error )

        ShowCode game ->
            ( { model | showingCode = Just game }, Cmd.none, Api.None )

        DoNothing ->
            ( model, Cmd.none, Api.None )


clearStatus : Model -> Model
clearStatus model =
    { model | statuses = [] }


renderStatus : Model -> Api.Status -> Html Msg
renderStatus model status =
    article
        [ A.class "target"
        , let
            -- Mess around with numbers for a "random"-ish number; `alpha` will
            -- become some float between 0 and 0.2
            number =
                List.sum (List.map Char.toCode (String.toList status.gameName))

            alpha =
                toFloat (modBy 793 number) / (793 / 0.2)
          in
          A.style "background-color" ("rgba(255, 0, 0, " ++ String.fromFloat alpha ++ ")")
        ]
        [ span [ A.class "leaderboard-link" ]
            [ text "See leaderboard for "
            , a [ A.class "game-link link", A.href ("?!" ++ status.game) ]
                [ text status.gameName ]
            ]
        , span [ A.class "flex" ] [ text " " ]
        , span [ A.class "target-label" ]
            [ text "Your target is" ]
        , a [ A.class "target-name link", A.href ("?@" ++ status.target) ]
            [ text status.targetName ]
        , button [ A.class "button kill-btn", onClick (ShowModal status.game) ]
            [ text "Eliminate" ]
        , case model.modal of
            Just modal ->
                if modal == status.game then
                    div [ A.class "modal-back show", onClick HideModal ]
                        [ form
                            [ A.class "modal kill-modal"
                            , stopPropagationOn "click" (D.succeed ( DoNothing, True ))
                            , onSubmit Kill
                            ]
                            [ Input.myInput ChangeCode
                                { myInputDefaults
                                    | labelText = "Target's elimination sequence"
                                    , placeholder = "hunter2"
                                    , value = model.code
                                    , id = Just "kill-modal-input"
                                    , attributes = [ A.attribute "autocapitalize" "none" ]
                                }
                            , input
                                [ A.class "button submit-btn"
                                , A.classList [ ( "loading", model.killing ) ]
                                , A.type_ "submit"
                                , A.value "Eliminate"
                                , A.disabled model.killing
                                ]
                                []
                            , case model.problem of
                                Just errorText ->
                                    span [ A.class "problematic-error" ]
                                        [ text errorText ]

                                Nothing ->
                                    text ""
                            ]
                        ]

                else
                    text ""

            _ ->
                text ""
        , span [ A.class "flex" ] [ text " " ]
        , span [ A.class "kill-code" ]
            [ text "Click to reveal and copy your elimination sequence:\n"
            , let
                showing =
                    case model.showingCode of
                        Just game ->
                            game == status.game

                        Nothing ->
                            False
              in
              span
                [ A.class "code copy-btn"
                , A.classList [ ( "revealed", showing ) ]
                , A.attribute "data-clipboard-text" status.code
                , onClick
                    (if showing then
                        DoNothing

                     else
                        ShowCode status.game
                    )
                ]
                [ text status.code ]
            , text "\n"
            , a [ A.class "link", A.href "?about#elimination-sequences" ]
                [ text "What is this for?" ]
            ]
        , span [ A.class "flex" ] [ text " " ]
        ]


renderOther : Api.OtherGame -> Html Msg
renderOther { game, gameName, state } =
    a [ A.class "other-game", A.href ("?!" ++ game) ]
        [ span [ A.class "other-game-name" ]
            [ text gameName ]
        , span
            [ A.class "other-game-status"
            , case state of
                Api.WillStart ->
                    A.class "will-start"

                Api.Started ->
                    A.class "started"

                Api.Ended ->
                    A.class "ended"
            ]
            [ text (Api.gameStateName state) ]
        ]


otherGameSorter : Api.OtherGame -> Api.OtherGame -> Order
otherGameSorter a b =
    let
        -- Ongoing games will be shown before awaiting player games
        -- because they're more interesting to check
        aVal =
            case a.state of
                Api.WillStart ->
                    1

                Api.Started ->
                    0

                Api.Ended ->
                    2

        bVal =
            case b.state of
                Api.WillStart ->
                    1

                Api.Started ->
                    0

                Api.Ended ->
                    2
    in
    case compare aVal bVal of
        EQ ->
            compare b.time a.time

        _ as order ->
            order


noStatuses : List Api.OtherGame -> String
noStatuses others =
    if List.isEmpty others then
        "To join a game, follow the link to a game page and click the Join button."

    else if List.any (\game -> game.state == Api.WillStart) others then
        "When the game starts, you'll see your target and code here."

    else if List.any (\game -> game.state == Api.Started) others then
        "You were eliminated! You can spectate by clicking on the game below."

    else
        "You aren't in any ongoing games. See the final results of previous games by clicking on them below."


view : Api.GlobalModel m -> Model -> List (Html Msg)
view global model =
    case global.session of
        Api.SignedIn _ ->
            [ div [ A.class "main targets" ] <|
                List.concat
                    [ [ a [ A.class "button small-screen-create-game-btn", A.href "?!bbdd6" ]
                            [ text "Gunn Elimination 2020" ]
                      , a [ A.class "button small-screen-create-game-btn", A.href "?create-game" ]
                            [ text "Create game" ]
                      ]
                    , if List.isEmpty model.statuses then
                        [ p [ A.class "no-statuses" ] [ text (noStatuses model.other) ] ]

                      else
                        List.map (renderStatus model) model.statuses
                    ]
            ]
                ++ (if List.isEmpty model.other then
                        []

                    else
                        [ div [ A.class "other-games-wrapper" ]
                            [ h2 [ A.class "other-games-header" ]
                                [ text "Other games you've joined" ]
                            , div [ A.class "other-games" ]
                                (model.other
                                    |> List.sortWith otherGameSorter
                                    |> List.map renderOther
                                )
                            ]
                        ]
                   )

        Api.SignedOut ->
            [ div [ A.class "main front-text" ]
                [ h1 [ A.class "website-title" ]
                    [ text "Elimination" ]
                , a [ A.class "button temp-btn", A.href "?!bbdd6" ]
                    [ text "Gunn Elimination 2020" ]
                ]
            , article [ A.class "main content welcome" ]
                [ p []
                    [ text "Elimination is a game involving hunting people down for words. "
                    , a [ A.class "link", A.href "?about" ]
                        [ text "Learn more." ]
                    ]
                , h2 [ A.class "stats-header" ]
                    [ text "Site-wide statistics" ]
                , div [ A.class "stats" ]
                    [ div [ A.class "stat" ]
                        [ span [ A.class "stat-name" ]
                            [ text "Global elimination count" ]
                        , span [ A.class "stat-value" ]
                            [ text (String.fromInt model.stats.kills) ]
                        ]
                    , div [ A.class "stat" ]
                        [ span [ A.class "stat-name" ]
                            [ text "Ongoing games" ]
                        , span [ A.class "stat-value" ]
                            [ text (String.fromInt model.stats.active) ]
                        ]
                    , div [ A.class "stat" ]
                        [ span [ A.class "stat-name" ]
                            [ text "Total games" ]
                        , span [ A.class "stat-value" ]
                            [ text (String.fromInt model.stats.games) ]
                        ]
                    ]
                ]
            ]
